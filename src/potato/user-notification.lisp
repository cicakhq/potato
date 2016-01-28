(in-package :potato.user-notification)

(declaim #.potato.common::*compile-decl*)

;;;
;;;  user-notification
;;;

(deftype user-notification-type () '(member :private :mention :word))

(defclass user-notification ()
  ((message            :type string
                       :initarg :message-id
                       :reader user-notification/message-id
                       :persisted-p t
                       :documentation "The message id of the message that caused the notification to be sent")
   (user               :type string
                       :initarg :user
                       :reader user-notification/user
                       :persisted-p t
                       :documentation "The user id of the recipient")
   (sender             :type string
                       :initarg :sender
                       :reader user-notification/sender
                       :persisted-p t
                       :documentation "The user id of the user that sent the message")
   (sender-description :type string
                       :initarg :sender-description
                       :reader user-notification/sender-description
                       :persisted-p t
                       :documentation "The full name of the user that sent the message")
   (channel            :type string
                       :initarg :channel
                       :reader user-notification/channel
                       :persisted-p t
                       :documentation "The channel the message was sent to")
   (text               :type string
                       :initarg :text
                       :reader user-notification/text
                       :persisted-p t
                       :documentation "The text content of the notification. This is the same as the content of the originating message.")
   (created-date       :type local-time:timestamp
                       :initarg :created-date
                       :reader user-notification/created-date
                       :persisted-p t
                       :persisted-type :date
                       :documentation "The timestamp when the notification was created")
   (notification-type  :type user-notification-type
                       :initarg :notification-type
                       :reader user-notification/notification-type
                       :persisted-p t
                       :persisted-type :symbol
                       :documentation "The notification type")
   (read               :type t
                       :initform nil
                       :accessor user-notification/read
                       :persisted-p t
                       :persisted-allow-missing-value t
                       :persisted-missing-default nil
                       :persisted-type :boolean
                       :documentation "Indicates if the notification has been read")
   (ping-timestamp     :type (or null local-time:timestamp)
                       :initarg :ping-timestamp
                       :initform nil
                       :accessor user-notification/ping-timestamp
                       :persisted-p t
                       :persisted-allow-missing-value t
                       :persisted-missing-default nil
                       :persisted-type :date
                       :documentation "If the notification remains unread after this time, a notification should be sent to the user. If NIL, then this message should not cause a ping to be sent to the recipient."))
  (:metaclass potato.db:persisted-entry-class)
  (:documentation "Object that stores a notification that is sent to a
user. The notification has a READ state, which indicates if the
notification should be considered 'active'. There is a separate thread
that regularly checks for notifications that have not been marked as
read, and will somehow notify the user that there are waiting
notifications. The typical way of doing so is to send an email."))

(defmethod print-object ((obj user-notification) stream)
  (print-unreadable-safely (message user notification-type) obj stream
    (format stream "MESSAGE ~s USER ~s TYPE ~s" message user notification-type)))

;;;
;;;  user-notification-state
;;;

(defclass user-notification-state ()
  ((user             :type string
                     :initarg :user
                     :reader user-notification-state/user
                     :persisted-p t)
   (email-sent-time  :type (or null local-time:timestamp)
                     :initarg :email-sent-time
                     :initform nil
                     :accessor user-notification-state/email-sent-time
                     :persisted-p t
                     :persisted-type :date
                     :documentation "The time the most recent notification email was sent")
   (block-email-send :type t
                     :initarg :block-email-send
                     :initform nil
                     :accessor user-notification-state/block-email-send
                     :persisted-p t
                     :persisted-type :boolean
                     :documentation "If true, an email has been sent but the user has not visited the site yet"))
  (:metaclass potato.db:persisted-entry-class)
  (:couchdb-type :|user_notification_email_state|)
  (:memcached-enabled-p t))

(defmethod initialize-instance :after ((obj user-notification-state) &key)
  (let ((sid (make-user-notification-state-id (user-notification-state/user obj))))
    (alexandria:if-let ((id (potato.db:persisted-entry/couchdb-id obj)))
      ;; Couchdb id is set, ensure it matches the user id
      (unless (string= sid id)
        (error "User notification state id doesn't match user id: ~s" id))
      ;; ELSE: Update the id of the instance
      (setf (potato.db:persisted-entry/couchdb-id obj) sid))))

(defmethod print-object ((obj user-notification-state) stream)
  (print-unreadable-safely (user email-sent-time block-email-send) obj stream
    (format stream "USER ~s EMAIL-SENT-TIME ~s BLOCK-EMAIL-SEND ~s" user email-sent-time block-email-send)))

(defun make-user-notification-state-id (user)
  (format nil "usernotificationstate-~a" (encode-name (potato.core:ensure-user-id user))))

(defun find-user-notification-state-for-user (user)
  (let* ((uid (potato.core:ensure-user-id user)))
    (with-user-notification-db
      (potato.db:load-instance 'user-notification-state (make-user-notification-state-id uid) :error-if-not-found nil))))

#+nil(defun update-last-notification-email-time (user)
  (let ((now (local-time:now))
        (uid (potato.core:ensure-user-id user)))
    (labels ((try-save (state)
               (setf (user-notification-state/email-sent-time state) now)
               (setf (user-notification-state/block-email-send state) t)
               (with-user-notification-db
                 (potato.db:save-instance state))))
      (alexandria:if-let ((state (find-user-notification-state-for-user user)))
        (try-save state)
        ;; ELSE: Try to create a new instance and save
        (let ((s2 (make-instance 'user-notification-state :user uid)))
          (handler-case
              (try-save s2)
            (clouchdb:id-or-revision-conflict () (try-save (find-user-notification-state-for-user user)))))))))

(defun load-min-email-delay-for-user (user)
  (let ((keywords (potato.core:load-notification-keywords-for-user user)))
    (potato.core:notification-keywords/min-email-delay keywords)))

(defun check-and-update-last-notification-date (user)
  (let ((now (local-time:now))
        (uid (potato.core:ensure-user-id user)))
    (labels ((try-save (state)
               (setf (user-notification-state/email-sent-time state) now)
               (setf (user-notification-state/block-email-send state) t)
               (with-user-notification-db
                 (potato.db:save-instance state))))
      (let ((state (find-user-notification-state-for-user user)))
        (cond ((null state) ;; No state, thus sending is always allowed
               (let ((s2 (make-instance 'user-notification-state :user uid)))
                 (handler-case
                     (try-save s2)
                   (clouchdb:id-or-revision-conflict () (try-save (find-user-notification-state-for-user user))))
                 t))
              ;; We have a state, check that sending is allowed
              ((or (null (user-notification-state/email-sent-time state))
                   (not (user-notification-state/block-email-send state))
                   (local-time:timestamp< (local-time:timestamp+ (user-notification-state/email-sent-time state)
                                                                 (load-min-email-delay-for-user user)
                                                                 :sec)
                                          now))
               (try-save state)
               t)
              ;; Email sending not allowed
              (t
               nil))))))

(defun clear-block-email-send-flag (user)
  (let ((state (find-user-notification-state-for-user user)))
    (when (and state
               (user-notification-state/block-email-send state))
      (setf (user-notification-state/block-email-send state) nil)
      (handler-case
          (with-user-notification-db
            (potato.db:save-instance state))
        (clouchdb:id-or-revision-conflict (condition)
          (log:warn "Conflict when updating block email send flag: ~a" condition))))))

(defun mark-notifications-by-id (user ids)
  (clear-block-email-send-flag user)
  (with-user-notification-db
    (let* ((uid (potato.core:ensure-user-id user))
           (args (list (cons "user" uid))))
      (dolist (id ids)
        (log:trace "Calling update mark_as_read. id=~s, args=~s" id args)
        (let ((result (potato.db:call-clouchdb-update-function "user" "mark_as_read" id args)))
          (unless (or (equal result "ok")
                      (equal result "already-marked-as-read"))
            (error "Error from mark_as_read update")))))))

(defun mark-notifications-for-user-channel (user channel)
  (with-user-notification-db
    (let* ((uid (potato.core:ensure-user-id user))
           (cid (potato.core:ensure-channel-id channel))
           (result (clouchdb:invoke-view "user" "notifications_for_user_channel"
                                         :start-key (list uid cid 'clouchdb:json-map)
                                         :end-key (list uid cid nil)
                                         :descending t
                                         :limit 1000 ; Limit in order to avoid flooding with broken db's
                                         )))
      (mark-notifications-by-id uid (mapcar (lambda (row)
                                              (first (getfield :|value| row)))
                                            (getfield :|rows| result))))))

(defun message-for-notification (user-id mapping match-keywords text)
  (cond ((eq (potato.core:channel-users/group-type mapping) :private)
         ;; All messages in a private channel triggers a notification
         :private)
        ((search (format nil "~cuser:~a:" (code-char #xf0001) user-id) text)
         ;; Direct mention of the user (using @-prefix)
         :mention)
        ((some (lambda (v) (search v text :test #'equalp)) match-keywords)
         ;; User-configurable notification keyword list
         :word)
        (t
         nil)))

(defun process-user-notifications-for-new-message (message mapping)
  (check-type message potato.core:message)
  (check-type mapping potato.core:channel-users)
  ;; Don't send notifications for deleted messages
  (unless (potato.core:message/deleted message)
    (let* ((text (potato.core:message/text message))
           (entries (potato.core:channel-users/users mapping))
           (match-keywords (potato.core:notification-keywords-for-users-in-channel (potato.core:channel-users/channel-id mapping)))
           (now (local-time:now)))
      (loop
         for entry in entries     ; Loop over all users in the channel
         for user-id = (car entry)
         for count = (getfield :|count| (cdr entry))
         ;; Don't send notifications for the messages sent by the user
         when (not (string= user-id (potato.core:message/from message)))
         ;; Get the notification-keywords instance for the user. The
         ;; notification-keywords object contains not only the
         ;; keywords but also all other notification configuration
         ;; information for the user.
         do (alexandria:when-let ((kw (cdr (assoc (symbol-name user-id) match-keywords :test #'equal))))
              (let ((user-match-keywords (potato.core:notification-keywords/keywords kw)))
                (log:trace "Check notifications for ~s, keywords: ~s" entry user-match-keywords)
                (alexandria:when-let ((notification-type (message-for-notification user-id mapping user-match-keywords text)))
                  (let* ((user-id (symbol-name (car entry)))
                         (channel-id (potato.core:message/channel message))
                         (ping-delay (potato.core:notification-keywords/ping-delay kw))
                         (ping-timestamp (if ping-delay
                                             (local-time:adjust-timestamp now (:offset :sec ping-delay))
                                             nil))
                         (notification (make-instance 'user-notification
                                                      :message-id (potato.core:message/id message)
                                                      :user user-id
                                                      :channel channel-id
                                                      :created-date (potato.core:message/created-date message)
                                                      :sender (potato.core:message/from message)
                                                      :sender-description (potato.core:message/from-name message)
                                                      :text text
                                                      :notification-type notification-type
                                                      :ping-timestamp ping-timestamp)))
                    (with-user-notification-db
                      (potato.db:save-instance notification))
                    (with-pooled-rabbitmq-connection (conn)
                      (let ((props `((:message-id . ,(potato.db:persisted-entry/couchdb-id notification))))
                            (created-date (potato.core:format-timestamp nil (user-notification/created-date notification))))
                        (cl-rabbit:basic-publish conn 1
                                                 :exchange *user-notifications-exchange-name*
                                                 :routing-key (format nil "~a.~a"
                                                                      channel-id
                                                                      (encode-name-for-routing-key user-id))
                                                 :properties props
                                                 :body (lisp-to-binary (list :user user-id
                                                                             :created-date created-date
                                                                             :message-id (potato.core:message/id message)
                                                                             :notification-id (potato.db:persisted-entry/couchdb-id notification)
                                                                             :text (potato.core:message/text message)
                                                                             :from (potato.core:message/from message)
                                                                             :from-name (potato.core:message/from-name message)
                                                                             :notification-type notification-type
                                                                             :channel (potato.core:message/channel message))))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RabbitMQ notifications system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-user-notification-message-and-build-json (content)
  (destructuring-bind (&key user created-date message-id notification-id &allow-other-keys) content
    (log:trace "Received user notification. user=~s, created-date=~s, message-id=~s, notification-id=~s"
               user created-date message-id notification-id)
    (with-user-notification-db
      (let* ((v (potato.db:load-instance 'potato.user-notification:user-notification notification-id)))
        (st-json:jso "type" "usernot"
                     "id" (potato.db:persisted-entry/couchdb-id v)
                     "user" (user-notification/sender v)
                     "user_description" (user-notification/sender-description v)
                     "channel" (user-notification/channel v)
                     "text" (user-notification/text v)
                     "created_date" (potato.core:format-timestamp nil (user-notification/created-date v))
                     "notification_type" (symbol-name (user-notification/notification-type v)))))))
