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

(defun flush-cached-gcm-keys-for-user-id (uid)
  (cl-memcached:mc-del (make-memcached-key-for-gcm-keys uid)))

(potato.db:define-hook-fn flush-cached-gcm-keys gcm-registration (obj :type (:save :delete))
  (flush-cached-gcm-keys-for-user-id (gcm-registration/user obj)))

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

(defun unregister-gcm-key (uid token)
  (clouchdb:delete-document (make-gcm-registration-key uid token))
  (flush-cached-gcm-keys-for-user-id uid))

(defun update-gcm-key (uid old-token new-token)
  (clouchdb:delete-document (make-gcm-registration-key uid old-token))
  (register-gcm uid new-token))

(defun process-single-reply (uid gcm-key result)
  (alexandria:if-let ((message-id (st-json:getjso "message_id" result)))
    ;; The message has been sent, but we may have to update the token
    (alexandria:when-let ((new-id (st-json:getjso "registration_id" result)))
      (update-gcm-key uid gcm-key new-id))
    ;; ELSE: Check error
    (alexandria:if-let ((error-message (st-json:getjso "error" result)))
      ;; Check the error and possibly unregister the key
      (string-case:string-case (error-message)
        ;; TODO: Resend the message (go through a delayed rabbitmq queue?)
        ("Unavailable"
         (log:warn "Need to resend message here: ~s" result))
        ("NotRegistered"
         (unregister-gcm-key uid gcm-key))
        (t
         (log:error "Unexpected error message from GCM request: ~s" result)
         (unregister-gcm-key uid gcm-key)))
      ;; ELSE: A GCM server reply should always contain either a message id or an error message
      (log:error "Unexpected reply from GCM request. user=~s, token=~s, result=~s" uid gcm-key result))))

(defun process-reply (uid gcm-key message)
  ;; If the result was successful, there is no need to do anything
  (unless (= (st-json:getjso "success" message) 1)
    (let ((results (st-json:getjso "results" message)))
      (if (not (alexandria:sequence-of-length-p results 1))
          ;; There should never be anything but a single entry in this list.
          (log:error "Unexpected result from GCM server: ~s" message)
          ;; ELSE: Check the result status
          (process-single-reply uid gcm-key (first results))))))

(defun push-gcm-message (uid gcm-key message-id data)
  (let ((content (st-json:jso "to" gcm-key
                              "message_id" message-id
                              "time_to_live" (* 4 24 60 60)
                              "data" data)))
    (log:debug "Content = ~s" content)
    (multiple-value-bind (body code headers orig-url stream should-close reason)
        (drakma:http-request "https://gcm-http.googleapis.com/gcm/send"
                             :method :post
                             :content-type "application/json"
                             :additional-headers `((:authorization . ,(concatenate 'string "key=" *gcm-authorisation-key*)))
                             :content (babel:string-to-octets (st-json:write-json-to-string content) :encoding :utf-8))
      (declare (ignore headers orig-url))
      (unwind-protect
           (if (= code hunchentoot:+http-ok+)
               ;; Successful delivery. We need to check whether the send was successful
               (process-reply uid gcm-key (st-json:read-json-from-string (babel:octets-to-string body :encoding :utf-8)))
               ;; ELSE: Error from service. We might want to push the message back on to the queue here?
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
    (destructuring-bind (&key user message-id from from-name text notification-type channel &allow-other-keys)
        (binary-to-lisp (cl-rabbit:message/body message))
      (loop
        for key in (gcm-keys-for-user user)
        do (log:info "Sending to key = ~s" key)
        do (push-gcm-message user key message-id
                             (st-json:jso "message_id" message-id
                                          "sender_id" from
                                          "sender_name" from-name
                                          "channel" channel
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
