(in-package :potato.ping-sender)

(declaim #.potato.common::*compile-decl*)

(defun make-channel-id-to-name-map (channel-ids)
  (let ((map (receptacle:make-hash-map :test 'equal)))
    (dolist (cid channel-ids)
      (unless (receptacle:hash-get map cid)
        (let ((channel (potato.db:load-instance 'potato.core:channel cid)))
          (setf (receptacle:hash-get map cid) (potato.core:channel/name channel)))))
    map))

(defun load-channels-for-notifications (notifications)
  (let ((cidlist (remove-duplicates (mapcar #'potato.user-notification:user-notification/channel notifications)
                                    :test #'equal)))
    (mapcar (lambda (cid)
              (cons cid (potato.db:load-instance 'potato.core:channel cid)))
            cidlist)))

(defun load-context (display-config notification)
  (check-type display-config potato.core:display-config)
  (check-type notification potato.user-notification:user-notification)
  (let* ((message-id (potato.user-notification:user-notification/message-id notification))
         (msglist (potato.core:load-message-range (potato.core:load-message message-id))))
    (loop
      for msg in msglist
      collect `((:message . ,(potato.core:message/id msg))
                (:sender-name . ,(potato.core:message/from-name msg))
                (:text . ,(format-message (potato.core:message/text msg)))
                (:date . ,(potato.core:format-timestamp-for-display-config display-config
                                                                           (potato.core:message/created-date msg)))
                (:highlighted . ,(equal (potato.core:message/id msg)
                                        (potato.user-notification:user-notification/message-id notification)))))))

(defun merge-message-lists (m0 m1)
  (let ((last-m0 (car (last m0))))
    (alexandria:if-let ((remaining-messages (member (getfield :message last-m0) m1
                                                    :key (lambda (msg) (getfield :message msg)) :test #'equal)))
      ;; The lists overlap
      (append m0 (cdr remaining-messages))
      ;; ELSE: The lists does not overlap, simply return nil
      nil)))

(defun make-notification-summary-for-channel (channel display-config notifications)
  (let ((result nil)
        (current nil))
    (dolist (notification notifications)
      (let ((msglist (load-context display-config notification)))
        (if current
            (alexandria:if-let ((merged-result (merge-message-lists current msglist)))
              (setq current merged-result)
              ;; ELSE: No merge, push the old result
              (progn
                (push (list (cons :messages current)) result)
                (setq msglist current)))
            ;; ELSE: This is the first loop
            (setq current msglist))))
    (push (list (cons :messages current)) result)
    `((:channel-url . ,(potato.core:make-potato-url "channel/~a" (potato.core:channel/id channel)))
      (:channel-name . ,(potato.core:channel/name channel))
      (:sections . ,(reverse result)))))

(defun make-channels-summary (notifications display-config)
  (let ((channels (load-channels-for-notifications notifications)))
    (mapcar (lambda (e)
              (make-notification-summary-for-channel
               (cdr e)
               display-config
               (remove-if-not (lambda (n)
                                (equal (car e) (potato.user-notification:user-notification/channel n)))
                              notifications)))
            channels)))

(defun send-notification-email (user notifications)
  (if (potato.user-notification:check-and-update-last-notification-date user)
      (if *debug*
          (log:info "sending email to ~s with ~a notifications" user (length notifications))
          ;; ELSE: Not debug, actually send the mail
          (let* ((n (length notifications))
                 (disp-conf (potato.core:load-display-config-for-user user))
                 (data `((:num-messages . ,n)
                         (:channels . ,(make-channels-summary notifications disp-conf))))
                 (content (flexi-streams:with-output-to-sequence (s)
                            (lofn:show-template s "new_messages_email.tmpl" data))))
            (potato.email:send-email (potato.email:make-mail-for-user user "You have new Potato notifications"
                                                                      (format nil "You have ~a new Potato notifications" n)
                                                                      (babel:octets-to-string content :encoding :utf-8)))))
      ;; ELSE: Email sending was blocked because the previous mail was sent too recently
      (log:trace "Email was not sent becasue a previous email was recently sent for user: ~s" (potato.core:ensure-user-id user))))

(defun process-email-notifications-for-user (user)
  (let* ((user (potato.core:ensure-user user))
         (result (with-user-notification-db
                   (potato.db:invoke-view-and-load-instances 'potato.user-notification:user-notification
                                                             "user" "notifications_for_user_unread"
                                                             :start-key (list (potato.core:user/id user) nil)
                                                             :end-key (list (potato.core:user/id user) 'clouchdb:json-map)))))
    (when result
      ;; Mark all notifications as read
      (dolist (n result)
        (setf (potato.user-notification:user-notification/read n) t)
        (with-user-notification-db
          (potato.db:save-instance n)))
      ;; Push the notification to rabbitmq
      #+nil(with-pooled-rabbitmq-connection (conn)
             )
      ;; Format an email and send it
      (send-notification-email user result))))

(defun ping-sender ()
  (loop
    for timestamp = (potato.core:format-timestamp nil (local-time:now))
    for result = (with-user-notification-db
                   (clouchdb:invoke-view "user" "notifications_updated"
                                         :end-key timestamp))
    for rows = (getfield :|rows| result)
    do (log:trace "Got ~d rows" (length rows))
    when rows
      do (let ((uids (make-hash-table :test 'equal)))
           (loop
             for row in rows
             do (setf (gethash (getfield :|value| row) uids) t))
           (log:trace "users with outstanding notifications: ~d" (hash-table-count uids))
           (loop
             for uid being each hash-key in uids
             do (process-email-notifications-for-user uid)))
    do (sleep 30)))

(defun start-ping-sender-thread ()
  (start-monitored-thread #'ping-sender "Ping sender loop"))
