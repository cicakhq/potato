(in-package :potato.gcm)

(declaim #.potato.common::*compile-decl*)

(defvar *gcm-authorisation-key* nil)
(defvar *gcm-sender* nil)

(deftype provider-type () '(member :gcm :apns :apns-dev))

(defclass gcm-registration ()
  ((user         :type string
                 :reader gcm-registration/user
                 :initarg :user
                 :persisted-p t
                 :documentation "User ID")
   (gcm-token    :type string
                 :reader gcm-registration/gcm-token
                 :initarg :gcm-token
                 :persisted-p t
                 :documentation "GCM registration key")
   (unread       :type list
                 :initarg :unread
                 :initform nil
                 :accessor gcm-registration/unread
                 :persisted-p t
                 :persisted-type (:list :string)
                 :persisted-allow-missing-value t)
   (notification :type list
                 :initarg :notification-channels
                 :initform nil
                 :accessor gcm-registration/notification-channels
                 :persisted-p t
                 :persisted-type (:list :string)
                 :persisted-allow-missing-value t)
   (provider     :type provider-type
                 :initarg :provider
                 :reader gcm-registration/provider
                 :persisted-p t
                 :persisted-type :symbol
                 :persisted-allow-missing-value t
                 :persisted-missing-default :gcm
                 :persisted-name :|recipient_type|))
  (:metaclass potato.db:persisted-entry-class))

(defun make-gcm-registration-key (user token provider)
  (check-type user (or string potato.core:user))
  (check-type token string)
  (check-type provider provider-type)
  (format nil "gcmkey-~a-~a-~a"
          (encode-name (string-downcase (symbol-name provider)))
          (encode-name (potato.core:ensure-user-id user))
          (encode-name token)))

(defmethod initialize-instance :after ((obj gcm-registration) &key)
  (let ((id (make-gcm-registration-key (gcm-registration/user obj)
                                       (gcm-registration/gcm-token obj)
                                       (gcm-registration/provider obj))))
    (cond ((null (potato.db:persisted-entry/couchdb-id obj))
           (setf (potato.db:persisted-entry/couchdb-id obj) id))
          ((string/= (potato.db:persisted-entry/couchdb-id obj) id)
           (error "GCM registration ID does not match user. id=~s, reg/user=~s, reg/token=~s, reg/provider=~s"
                  (potato.db:persisted-entry/couchdb-id obj)
                  (gcm-registration/user obj)
                  (gcm-registration/gcm-token obj)
                  (gcm-registration/provider obj))))))

(defun parse-provider-name (name)
  (string-case:string-case (name)
    ("gcm" :gcm)
    ("apns" :apns)
    ("apns-dev" :apns-dev)))

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
        for values = (getfield :|value| row)
        collect (list (first values) (intern (second values) "KEYWORD"))))))

(defun gcm-enabled-p ()
  (if *gcm-authorisation-key* t nil))

(defun register-gcm (user token provider)
  (check-type provider provider-type)
  (let* ((uid (potato.core:ensure-user-id user))
         (id (make-gcm-registration-key uid token provider))
         (reg (potato.db:load-instance 'gcm-registration id :error-if-not-found nil)))
    (labels ((make-and-save-token ()
               (let ((new-reg (make-instance 'gcm-registration :user uid :gcm-token token :provider provider)))
                 (potato.db:save-instance new-reg))))
      (cond ((not reg)
             (make-and-save-token)
             :new-registration)
            ((string/= (gcm-registration/gcm-token reg) token)
             (error "Attempt to re-register a GCM token.")
             ;; This code has been disabled since it would appear that
             ;; it could never actually have been called. I think this
             ;; is a leftover from older code where there could only
             ;; be a single GCM registration per user. Before
             ;; completely removing it, I'm keeping this comment here
             ;; until it can be proven that this will never be called.
             #+broken
             (progn
               (potato.db:remove-instance reg)
               (make-and-save-token))
             :token-updated)
            (t
             :not-changed)))))

(defun unregister-gcm-key (uid token provider)
  (clouchdb:delete-document (make-gcm-registration-key uid token provider))
  (flush-cached-gcm-keys-for-user-id uid))

(defun copy-registration (uid reg token)
  (make-instance 'gcm-registration
                 :user uid
                 :gcm-token token
                 :unread (gcm-registration/unread reg)
                 :notification-channels (gcm-registration/notification-channels reg)))

(defun update-gcm-key (uid old-token new-token provider)
  (let* ((old-key (make-gcm-registration-key uid old-token provider))
         (reg (potato.db:load-instance 'gcm-registration old-key :error-if-not-found nil)))
    (let ((new-reg (if reg
                       (progn
                         (clouchdb:delete-document old-key)
                         (copy-registration uid reg new-token))
                       (make-instance 'gcm-registration :user uid :gcm-token new-token))))
      (potato.db:save-instance new-reg))))

(defun update-unread-subscription (user token provider channel add-p)
  (let ((cid (potato.core:ensure-channel-id channel))
        (reg (potato.db:load-instance 'gcm-registration
                                      (make-gcm-registration-key (potato.core:ensure-user-id user) token provider)
                                      :error-if-not-found nil)))
    (unless reg
      (potato.core:raise-not-found-error "Incorrect GCM registration"))
    (if add-p
        (pushnew cid (gcm-registration/unread reg) :test #'equal)
        (setf (gcm-registration/unread reg) (remove cid (gcm-registration/unread reg) :test #'equal)))
    (potato.db:save-instance reg)
    reg))

(defun process-single-reply (uid gcm-key result)
  (alexandria:if-let ((message-id (st-json:getjso "message_id" result)))
    ;; The message has been sent, but we may have to update the token
    (alexandria:when-let ((new-id (st-json:getjso "registration_id" result)))
      (update-gcm-key uid gcm-key new-id :gcm))
    ;; ELSE: Check error
    (alexandria:if-let ((error-message (st-json:getjso "error" result)))
      ;; Check the error and possibly unregister the key
      (string-case:string-case (error-message)
        ;; TODO: Resend the message (go through a delayed rabbitmq queue?)
        ("Unavailable"
         (log:warn "Need to resend message here: ~s" result))
        ("NotRegistered"
         (unregister-gcm-key uid gcm-key :gcm))
        (t
         (log:error "Unexpected error message from GCM request: ~s" result)
         (unregister-gcm-key uid gcm-key :gcm)))
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

(defun push-gcm-message (uid gcm-key data)
  (let ((content (st-json:jso "to" gcm-key
                              "time_to_live" (* 4 24 60 60)
                              "data" data)))
    (log:debug "Content = ~s" content)
    (multiple-value-bind (body code headers orig-url stream should-close reason)
        (drakma:http-request "https://fcm.googleapis.com/fcm/send"
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

(defun process-gcm-user-notification (msg)
  (let ((message (cl-rabbit:envelope/message msg)))
    (destructuring-bind (&key user message-id from from-name text notification-type channel &allow-other-keys)
        (binary-to-lisp (cl-rabbit:message/body message))
      (loop
        for (key provider) in (gcm-keys-for-user user)
        do (log:info "Sending to key = ~s" key)
        do (case provider
             (:gcm
              (push-gcm-message user key (st-json:jso "potato_message_type" "message"
                                                      "message_id" message-id
                                                      "sender_id" from
                                                      "sender_name" from-name
                                                      "channel" channel
                                                      "notification_type" (symbol-name notification-type)
                                                      "text" (truncate-string text 1000))))
             (t
              (with-pooled-rabbitmq-connection (conn)
                (let* ((provider-name (string-downcase (symbol-name provider)))
                       (json (st-json:jso "potato_message_type" "message"
                                          "message_id" message-id
                                          "sender_id" from
                                          "sender_name" from-name
                                          "channel" channel
                                          "notification_type" (symbol-name notification-type)
                                          "text" (truncate-string text 1000)
                                          "provider" provider-name
                                          "token" key
                                          "user" (potato.core:ensure-user-id user))))
                  (cl-rabbit:basic-publish conn 1
                                           :exchange *apns-exchange-name*
                                           :routing-key (format nil "~a.~a" channel provider-name)
                                           :properties `((:message-id . ,message-id))
                                           :body (babel:string-to-octets (st-json:write-json-to-string json)))))))))))

(defun process-unread (msg)
  (let ((message (cl-rabbit:envelope/message msg)))
    (destructuring-bind (cid users)
        (binary-to-lisp (cl-rabbit:message/body message))
      (let ((res (clouchdb:invoke-view "gcm" "unread_channel" :key cid)))
        (loop
          for row in (getfield :|rows| res)
          for v = (getfield :|value| row)
          for uid = (first v)
          for token = (second v)
          for e = (find uid users :key #'first :test #'equal)
          when e
            do (push-gcm-message uid token (st-json:jso "potato_message_type" "unread"
                                                        "channel" cid
                                                        "unread" (princ-to-string (second e)))))))))

(defun gcm-listener-loop ()
  (with-rabbitmq-connected (conn)
    (cl-rabbit:basic-consume conn 1 *gcm-queue-name* :no-ack t)
    (loop
       for msg = (cl-rabbit:consume-message conn)
       do (string-case:string-case ((cl-rabbit:envelope/exchange msg))
            (#.*user-notifications-exchange-name* (process-gcm-user-notification msg))
            (#.*gcm-unread-state-exchange-name* (process-unread msg))))))

(defun start-gcm-listener ()
  (start-monitored-thread #'gcm-listener-loop "GCM listener loop"))

(defun gcm-admin-listener-loop ()
  (with-rabbitmq-connected (conn)
    (cl-rabbit:basic-consume conn 1 *apns-management-queue-name* :no-ack t)
    (loop
      for msg = (cl-rabbit:consume-message conn)
      for msg-body = (cl-rabbit:message/body (cl-rabbit:envelope/message msg))
      for json = (st-json:read-json-from-string (babel:octets-to-string msg-body :encoding :utf-8))
      do (json-bind ((cmd "cmd" :required t)
                     (uid "user" :required t)
                     (token "token" :required t)
                     (provider-name "provider" :required t))
             json
           (string-case:string-case (cmd)
             ("delete"
              (let ((reg (potato.db:load-instance 'gcm-registration
                                                  (make-gcm-registration-key uid token (parse-provider-name provider-name))
                                                  :error-if-not-found nil)))
                (if reg
                    (potato.db:remove-instance reg)
                    ;; ELSE: Registration did not exist, just log a message.
                    (log:warn "Attempt to remove a nonexistent registration: user=~s, token=~s, provider=~s"
                              uid token provider-name)))))))))

(defun start-gcm-admin-listener ()
  (start-monitored-thread #'gcm-admin-listener-loop "GCM admin listener loop"))

(potato.common.application:define-component gcm-sender
  (:dependencies potato.common::generic potato.db::db)
  (:start
   (unless (gcm-enabled-p)
     (log:warn "GCM key not configured. GCM notifications will not be available."))
   (start-gcm-listener)
   (start-gcm-admin-listener)))
