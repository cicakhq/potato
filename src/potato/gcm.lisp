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

(defun push-gcm-message (gcm-key message-id)
  (let ((content (st-json:jso "to" gcm-key
                              "message_id" message-id
                              "data" (st-json:jso "field1" "abc" "field2" "otherdata"))))
    (drakma:http-request "https://gcm-http.googleapis.com/gcm/send"
                         :method :post
                         :content-type "application/json"
                         :content (st-json:write-json-to-string content)
                         :additional-headers `((:authorization . "key=AIzaSyBVxTvHCGVd-a0HVHzKYKb-eWsE7MeSRr4")))))

(defun process-gcm-user-notification (msg)
  (let ((message (cl-rabbit:envelope/message msg)))
    (destructuring-bind (user-id created-date message-id)
        (read-from-string (babel:octets-to-string (cl-rabbit:message/body message) :encoding :utf-8))
      (declare (ignore created-date))
      (let ((user (potato.db:load-instance 'potato.core:user user-id)))
        (alexandria:when-let ((gcm-key (potato.core:user/android-gcm-key user)))
          (push-gcm-message gcm-key message-id))))))

(defun gcm-listener-loop ()
  (with-rabbitmq-connected (conn)
    (cl-rabbit:basic-consume conn 1 *gcm-queue-name* :no-ack t)
    (loop
       for msg = (cl-rabbit:consume-message conn)
       do (process-gcm-user-notification msg))))

(defun start-gcm-listener ()
  (start-monitored-thread #'gcm-listener-loop "GCM listener loop"))
