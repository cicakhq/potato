(in-package :potato.gcm)

(declaim #.potato.common::*compile-decl*)

(defvar *gcm-authorisation-key* nil)

(defclass gcm-registration ()
  ((user      :type string
              :reader gcm-registration/user
              :initarg :user
              :persisted-p t
              :documentation "User ID")
   (gcm-token :type string
              :reader gcm-registration/gcm-token
              :initarg :gcm-token
              :persisted-p t
              :documentation "GCM registration key"))
  (:metaclass potato.db:persisted-entry-class))

(defun make-gcm-registration-key (user token)
  (concatenate 'string "gcmkey-" (encode-name (potato.core:ensure-user-id user)) "-" (encode-name token)))

(defmethod initialize-instance :after ((obj gcm-registration) &key)
  (let ((id (make-gcm-registration-key (gcm-registration/user obj) (gcm-registration/gcm-token obj))))
    (cond ((null (potato.db:persisted-entry/couchdb-id obj))
           (setf (potato.db:persisted-entry/couchdb-id obj) id))
          ((string/= (potato.db:persisted-entry/couchdb-id obj) id)
           (error "GCM registration ID does not match user. id=~s, reg/user=~s, reg/token=~s"
                  (potato.db:persisted-entry/couchdb-id obj)
                  (gcm-registration/user obj)
                  (gcm-registration/gcm-token obj))))))

(defun make-memcached-key-for-gcm-keys (uid)
  (format nil "gcm-~a" (encode-name uid)))

(defun gcm-keys-for-user (uid)
  (potato.common.memcached:with-memcached (make-memcached-key-for-gcm-keys uid)
    (let ((result (clouchdb:invoke-view "gcm" "gcm_for_user" :key uid)))
      (loop
        for row in (getfield :|rows| result)
        collect (getfield :|value| row)))))

(defun gcm-enabled-p ()
  (if *gcm-authorisation-key* t nil))

(defun register-gcm (user token)
  (let* ((uid (potato.core:ensure-user-id user))
         (id (make-gcm-registration-key uid token))
         (reg (potato.db:load-instance 'gcm-registration id :error-if-not-found nil)))
    (labels ((make-and-save-token ()
               (let ((new-reg (make-instance 'gcm-registration :user uid :gcm-token token)))
                 (potato.db:save-instance new-reg))))
      (cond ((not reg)
             (make-and-save-token)
             :new-registration)
            ((string/= (gcm-registration/gcm-token reg) token)
             (potato.db:remove-instance reg)
             (make-and-save-token)
             :token-updated)
            (t
             :not-changed)))))

(defun push-gcm-message (gcm-key message-id data)
  (let ((content (st-json:jso "to" gcm-key
                              "message_id" message-id
                              "data" data)))
    ;;(setq content (st-json:jso "to" gcm-key "data" (st-json:jso "user" "uu" "from" "koko")))
    (log:debug "Content = ~s" content)
    (multiple-value-bind (body code headers orig-url stream should-close reason)
        (drakma:http-request "https://gcm-http.googleapis.com/gcm/send"
                             :method :post
                             :content-type "application/json"
                             :additional-headers `((:authorization . ,(concatenate 'string "key=" *gcm-authorisation-key*)))
                             :content (babel:string-to-octets (st-json:write-json-to-string content) :encoding :utf-8))
      (declare (ignore body headers orig-url))
      (unwind-protect
           (unless (= code hunchentoot:+http-ok+)
             (log:error "Error sending message to GCM service. code=~s, reason=~s" code reason))
        (when should-close (close stream))
        t))))

(defun maybe-truncate-text (text)
  (if (> (length text) 1000)
      ;; TODO: Need to cut at the appropriate grapheme cluster boundary
      (subseq text 0 1000)
      text))

(defun process-gcm-user-notification (msg)
  (let ((message (cl-rabbit:envelope/message msg)))
    (destructuring-bind (&key user message-id from from-name text notification-type &allow-other-keys)
        (binary-to-lisp (cl-rabbit:message/body message))
      (loop
        for key in (gcm-keys-for-user user)
        do (log:info "Sending to key = ~s" key)
        do (push-gcm-message key message-id (st-json:jso "message_id" message-id
                                                         "sender_id" from
                                                         "sender_name" from-name
                                                         "notification_type" (symbol-name notification-type)
                                                         "text" (maybe-truncate-text text)))))))

(defun gcm-listener-loop ()
  (with-rabbitmq-connected (conn)
    (cl-rabbit:basic-consume conn 1 *gcm-queue-name* :no-ack t)
    (loop
       for msg = (cl-rabbit:consume-message conn)
       do (process-gcm-user-notification msg))))

(defun start-gcm-listener ()
  (start-monitored-thread #'gcm-listener-loop "GCM listener loop"))

(potato.common.application:define-component gcm-sender
  (:dependencies potato.common::generic potato.db::db)
  (:start
   (if (gcm-enabled-p)
       (start-gcm-listener)
       (log:warn "GCM key not configured. Not starting GCM sender."))))
