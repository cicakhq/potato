(in-package :potato.core)

(declaim #.potato.common::*compile-decl*)

(defclass notification-keywords (potato.db:persisted-entry)
  ((user                   :type string
                           :initarg :user
                           :reader notification-keywords/user
                           :persisted-p t
                           :documentation "The use-id of the user")
   (message-match-keywords :type list
                           :initarg :keywords
                           :initform nil
                           :accessor notification-keywords/keywords
                           :persisted-p t
                           :persisted-type (:list :string)
                           :documentation "A list of words that will trigger a notification for the user")
   (ping-delay             :type (or null integer)
                           :initarg :ping-delay
                           :initform 300 ; 5 minutes
                           :accessor notification-keywords/ping-delay
                           :persisted-p t
                           :persisted-type :integer
                           :persisted-allow-missing-value t
                           :persisted-missing-default nil
                           :documentation "The time after a notification was sent to a user until an email
reminder is sent.")
   (min-email-delay        :type (or null integer)
                           :initarg :min-email-delay
                           :initform #.(* 2 60 60)
                           :accessor notification-keywords/min-email-delay
                           :persisted-p t
                           :persisted-type :integer
                           :persisted-allow-missing-value t
                           :persisted-missing-default #.(* 2 60 60)
                           :documentation "The minimum time between notification emails"))
  (:metaclass potato.db:persisted-entry-class)
  (:attachments-p nil)
  (:memcached-enabled-p t)
  (:documentation "Keywords that trigger user notifications on a channel"))

(defmethod initialize-instance :after ((obj notification-keywords) &key)
  (let ((id (make-notification-keywords-id (notification-keywords/user obj))))
    (cond ((null (potato.db:persisted-entry/couchdb-id obj))
           (setf (potato.db:persisted-entry/couchdb-id obj) id))
          ((string/= (potato.db:persisted-entry/couchdb-id obj) id)
           (error "Notification keywords ID does not match user. id=~s, user=~s"
                  (potato.db:persisted-entry/couchdb-id obj) (notification-keywords/user obj))))))

(defun make-notification-keywords-id (user)
  (format nil "messagekeywords-~a" (ensure-user-id user)))

(defun make-notification-keywords-memcached-key (channel-id)
  (concatenate 'string "notification-keywords-" channel-id))

(defun load-notification-keywords-for-user (user)
  (or (potato.db:load-instance 'notification-keywords (make-notification-keywords-id user) :error-if-not-found nil)
      (let* ((user (ensure-user user))
             (keywords (make-instance 'notification-keywords :user (user/id user))))
        (potato.db:save-instance keywords)
        keywords)))

(defun %notification-keywords-for-users-in-channel (channel-id)
  (let ((result (clouchdb:invoke-view "channel" "users_in_channel" :key channel-id)))
    (loop
       for row in (getfield :|rows| result)
       collect (let* ((user-id (getfield :|_id| (getfield :|value| row)))
                      (config (load-notification-keywords-for-user user-id)))
                 (cons user-id config)))))

(defun notification-keywords-for-users-in-channel (channel)
  (let* ((channel-id (ensure-channel-id channel))
         (memcached-key (make-notification-keywords-memcached-key channel))
         (cached (cl-memcached:mc-get (list memcached-key))))
    (if cached
        (potato.common:decode-conspack-with-interning (fifth (car cached)))
        (let ((keywords (%notification-keywords-for-users-in-channel channel-id)))
          (cl-memcached:mc-set memcached-key (conspack:encode keywords))
          keywords))))

(potato.db:define-hook-fn flush-notification-keywords notification-keywords (config)
  (let ((result (clouchdb:invoke-view "user" "channels_by_domain_and_user"
                                      :start-key (list (notification-keywords/user config) nil)
                                      :end-key (list (notification-keywords/user config) 'clouchdb:json-map))))
    (dolist (row (getfield :|rows| result))
      (cl-memcached:mc-del (make-notification-keywords-memcached-key (first (getfield :|value| row)))))))

;;;
;;;  Web API for manipulating the keywords
;;;

(define-json-handler-fn-login (load-notification-keywords-screen "/load_notification_keywords" data nil ())
  (declare (ignore data))
  (let ((keywords (load-notification-keywords-for-user (current-user))))
    (st-json:jso "keywords" (notification-keywords/keywords keywords))))

(define-json-handler-fn-login (update-notification-keywords-screen "/update_notification_keywords" data nil ())
  (let ((keywords (load-notification-keywords-for-user (current-user)))
        (updated (st-json:getjso "keywords" data)))
    (unless (and (listp updated)
                 (every #'stringp updated))
      (error "Illegal keyword list format"))
    (setf (notification-keywords/keywords keywords) updated)
    (potato.db:save-instance keywords)
    (st-json:jso "result" "ok")))
