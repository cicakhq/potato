(in-package :potato.ping-sender)

(declaim #.potato.common::*compile-decl*)

(defun make-channel-id-to-name-map (channel-ids)
  (let ((map (dhs-sequences:make-hash-map :test 'equal)))
    (dolist (cid channel-ids)
      (unless (dhs-sequences:hash-get map cid)
        (let ((channel (potato.db:load-instance 'potato.core:channel cid)))
          (setf (dhs-sequences:hash-get map cid) (potato.core:channel/name channel)))))
    map))

(defun load-channels-for-notifications (notifications)
  (let ((cidlist (remove-duplicates (mapcar #'potato.user-notification:user-notification/channel notifications)
                                    :test #'equal)))
    (mapcar (lambda (cid)
              (cons cid (potato.db:load-instance 'potato.core:channel cid)))
            cidlist)))

(defun load-context (notification)
  (let* ((message-id (potato.user-notification:user-notification/message-id notification))
         (msglist (potato.core:load-message-range-with-check message-id)))
    (loop
      for msg in msglist
      collect `((:message . ,(potato.core:message/id msg))
                (:sender-name . ,(potato.core:message/from-name msg))
                (:text . ,(format-message (potato.core:message/text msg)))
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

(defun make-notification-summary-for-channel (channel notifications)
  (let ((result nil)
        (current nil))
    (dolist (notification notifications)
      (let ((msglist (load-context notification)))
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

(defun make-channels-summary (notifications)
  (let ((channels (load-channels-for-notifications notifications)))
    (mapcar (lambda (e)
              (make-notification-summary-for-channel
               (cdr e)
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
                 (data `((:num-messages . ,n)
                         (:channels . ,(make-channels-summary notifications))))
                 (content (flexi-streams:with-output-to-sequence (s)
                            (lofn:show-template s "new_messages_email2.tmpl" data))))
            (potato.email:send-email (potato.email:make-mail-for-user user "You have new Potato notifications"
                                                                      (format nil "You have ~a new Potato notifications" n)
                                                                      (babel:octets-to-string content :encoding :utf-8)))))
      ;; ELSE: Email sending was blocked because the previous mail was sent too recently
      (log:trace "Email was not sent becasue a previous email was recently sent for user: ~s" (potato.core:ensure-user-id user))))

(defun send-notification-email-old (user notifications)
  (if (potato.user-notification:check-and-update-last-notification-date user)
      (if *debug*
          (log:info "sending email to ~s with ~a notifications" user (length notifications))
          ;; ELSE: Not debug, actually send the mail
          (let* ((n (length notifications))
                 (channel-names (make-channel-id-to-name-map (mapcar #'potato.user-notification:user-notification/channel
                                                                     notifications)))
                 (data `((:num-messages . ,n)
                         (:remaining-messages . ,(max (- n 10) 0))
                         (:messages . ,(loop
                                          for n in notifications
                                          repeat 10
                                          for sender-name = (potato.user-notification:user-notification/sender-description n)
                                          for channel-id = (potato.user-notification:user-notification/channel n)
                                          for channel-name = (dhs-sequences:hash-get channel-names channel-id)
                                          for channel-url = (potato.core:make-potato-url "channel/~a" channel-id)
                                          for text = (potato.user-notification:user-notification/text n)
                                          collect `((:sender-name  . ,sender-name)
                                                    (:channel-name . ,channel-name)
                                                    (:channel-url  . ,channel-url)
                                                    (:text         . ,(format-message text)))))))
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
                                          :reduce t
                                          :end-key timestamp))
     for rows = (getfield :|rows| result)
     when rows
     do (progn
          (if (cdr rows)
              (log:error "More than one result from reduced view: ~s" rows)
              ;; ELSE: All is good, process all the updated users
              (let ((uids (getfield :|value| (car rows))))
                (log:trace "Got notifications for users: ~s" uids)
                (loop
                   for uid in uids
                   do (process-email-notifications-for-user uid)))))
     do (sleep 10)))

(defun start-ping-sender-thread ()
  (start-monitored-thread #'ping-sender "Ping sender loop"))

(defparameter *test-data*
  '((:NUM-MESSAGES . 4)
 (:CHANNELS
  ((:CHANNEL-URL
    . "http://localhost:8080/channel/4e8ff1e0e1c7d80e8e4e5cea510147db")
   (:CHANNEL-NAME . "Default channel")
   (:SECTIONS
    ((:MESSAGES
      ((:MESSAGE
        . "msg-4e8ff1e0e1c7d80e8e4e5cea510147db-2015-12-10T07:35:42.189081Z_00000")
       (:SENDER-NAME . "Elias Mårtenson") (:TEXT . "<p>j</p>") (:HIGHLIGHTED))
      ((:MESSAGE
        . "msg-4e8ff1e0e1c7d80e8e4e5cea510147db-2015-12-10T07:35:43.312368Z_00000")
       (:SENDER-NAME . "Elias Mårtenson") (:TEXT . "<p>k</p>") (:HIGHLIGHTED))
      ((:MESSAGE
        . "msg-4e8ff1e0e1c7d80e8e4e5cea510147db-2015-12-10T07:35:43.759044Z_00000")
       (:SENDER-NAME . "Elias Mårtenson") (:TEXT . "<p>l</p>") (:HIGHLIGHTED))
      ((:MESSAGE
        . "msg-4e8ff1e0e1c7d80e8e4e5cea510147db-2015-12-10T07:35:44.639001Z_00000")
       (:SENDER-NAME . "Elias Mårtenson") (:TEXT . "<p>m</p>") (:HIGHLIGHTED))
      ((:MESSAGE
        . "msg-4e8ff1e0e1c7d80e8e4e5cea510147db-2015-12-10T07:35:51.342444Z_00000")
       (:SENDER-NAME . "Elias Mårtenson")
       (:TEXT
        . "<p>Message for <em class=\"user\" user-id=\"user-fc77a2aeb8bbbf4d194a\">Foo</em></p>")
       (:HIGHLIGHTED . T))
      ((:MESSAGE
        . "msg-4e8ff1e0e1c7d80e8e4e5cea510147db-2015-12-10T07:35:56.435783Z_00000")
       (:SENDER-NAME . "Elias Mårtenson")
       (:TEXT
        . "<p>message 2 for <em class=\"user\" user-id=\"user-fc77a2aeb8bbbf4d194a\">Foo</em></p>")
       (:HIGHLIGHTED))
      ((:MESSAGE
        . "msg-4e8ff1e0e1c7d80e8e4e5cea510147db-2015-12-10T07:35:57.798952Z_00000")
       (:SENDER-NAME . "Elias Mårtenson") (:TEXT . "<p>another</p>")
       (:HIGHLIGHTED))
      ((:MESSAGE
        . "msg-4e8ff1e0e1c7d80e8e4e5cea510147db-2015-12-10T07:35:59.479076Z_00000")
       (:SENDER-NAME . "Elias Mårtenson") (:TEXT . "<p>message</p>")
       (:HIGHLIGHTED))
      ((:MESSAGE
        . "msg-4e8ff1e0e1c7d80e8e4e5cea510147db-2015-12-10T07:36:04.045777Z_00000")
       (:SENDER-NAME . "Elias Mårtenson")
       (:TEXT
        . "<p>message 3 for <em class=\"user\" user-id=\"user-fc77a2aeb8bbbf4d194a\">Foo</em></p>")
       (:HIGHLIGHTED))
      ((:MESSAGE
        . "msg-4e8ff1e0e1c7d80e8e4e5cea510147db-2015-12-10T07:36:15.849007Z_00000")
       (:SENDER-NAME . "Elias Mårtenson")
       (:TEXT . "<p>and now a bigger separation</p>") (:HIGHLIGHTED))
      ((:MESSAGE
        . "msg-4e8ff1e0e1c7d80e8e4e5cea510147db-2015-12-10T07:36:16.292471Z_00000")
       (:SENDER-NAME . "Elias Mårtenson") (:TEXT . "<p>1</p>") (:HIGHLIGHTED))
      ((:MESSAGE
        . "msg-4e8ff1e0e1c7d80e8e4e5cea510147db-2015-12-10T07:36:16.525702Z_00000")
       (:SENDER-NAME . "Elias Mårtenson") (:TEXT . "<p>2</p>") (:HIGHLIGHTED))
      ((:MESSAGE
        . "msg-4e8ff1e0e1c7d80e8e4e5cea510147db-2015-12-10T07:36:16.782323Z_00000")
       (:SENDER-NAME . "Elias Mårtenson") (:TEXT . "<p>3</p>")
       (:HIGHLIGHTED))))
    ((:MESSAGES
      ((:MESSAGE
        . "msg-4e8ff1e0e1c7d80e8e4e5cea510147db-2015-12-10T07:35:42.189081Z_00000")
       (:SENDER-NAME . "Elias Mårtenson") (:TEXT . "<p>j</p>") (:HIGHLIGHTED))
      ((:MESSAGE
        . "msg-4e8ff1e0e1c7d80e8e4e5cea510147db-2015-12-10T07:35:43.312368Z_00000")
       (:SENDER-NAME . "Elias Mårtenson") (:TEXT . "<p>k</p>") (:HIGHLIGHTED))
      ((:MESSAGE
        . "msg-4e8ff1e0e1c7d80e8e4e5cea510147db-2015-12-10T07:35:43.759044Z_00000")
       (:SENDER-NAME . "Elias Mårtenson") (:TEXT . "<p>l</p>") (:HIGHLIGHTED))
      ((:MESSAGE
        . "msg-4e8ff1e0e1c7d80e8e4e5cea510147db-2015-12-10T07:35:44.639001Z_00000")
       (:SENDER-NAME . "Elias Mårtenson") (:TEXT . "<p>m</p>") (:HIGHLIGHTED))
      ((:MESSAGE
        . "msg-4e8ff1e0e1c7d80e8e4e5cea510147db-2015-12-10T07:35:51.342444Z_00000")
       (:SENDER-NAME . "Elias Mårtenson")
       (:TEXT
        . "<p>Message for <em class=\"user\" user-id=\"user-fc77a2aeb8bbbf4d194a\">Foo</em></p>")
       (:HIGHLIGHTED . T))
      ((:MESSAGE
        . "msg-4e8ff1e0e1c7d80e8e4e5cea510147db-2015-12-10T07:35:56.435783Z_00000")
       (:SENDER-NAME . "Elias Mårtenson")
       (:TEXT
        . "<p>message 2 for <em class=\"user\" user-id=\"user-fc77a2aeb8bbbf4d194a\">Foo</em></p>")
       (:HIGHLIGHTED))
      ((:MESSAGE
        . "msg-4e8ff1e0e1c7d80e8e4e5cea510147db-2015-12-10T07:35:57.798952Z_00000")
       (:SENDER-NAME . "Elias Mårtenson") (:TEXT . "<p>another</p>")
       (:HIGHLIGHTED))
      ((:MESSAGE
        . "msg-4e8ff1e0e1c7d80e8e4e5cea510147db-2015-12-10T07:35:59.479076Z_00000")
       (:SENDER-NAME . "Elias Mårtenson") (:TEXT . "<p>message</p>")
       (:HIGHLIGHTED))
      ((:MESSAGE
        . "msg-4e8ff1e0e1c7d80e8e4e5cea510147db-2015-12-10T07:36:04.045777Z_00000")
       (:SENDER-NAME . "Elias Mårtenson")
       (:TEXT
        . "<p>message 3 for <em class=\"user\" user-id=\"user-fc77a2aeb8bbbf4d194a\">Foo</em></p>")
       (:HIGHLIGHTED))
      ((:MESSAGE
        . "msg-4e8ff1e0e1c7d80e8e4e5cea510147db-2015-12-10T07:36:15.849007Z_00000")
       (:SENDER-NAME . "Elias Mårtenson")
       (:TEXT . "<p>and now a bigger separation</p>") (:HIGHLIGHTED))
      ((:MESSAGE
        . "msg-4e8ff1e0e1c7d80e8e4e5cea510147db-2015-12-10T07:36:16.292471Z_00000")
       (:SENDER-NAME . "Elias Mårtenson") (:TEXT . "<p>1</p>") (:HIGHLIGHTED))
      ((:MESSAGE
        . "msg-4e8ff1e0e1c7d80e8e4e5cea510147db-2015-12-10T07:36:16.525702Z_00000")
       (:SENDER-NAME . "Elias Mårtenson") (:TEXT . "<p>2</p>") (:HIGHLIGHTED))
      ((:MESSAGE
        . "msg-4e8ff1e0e1c7d80e8e4e5cea510147db-2015-12-10T07:36:16.782323Z_00000")
       (:SENDER-NAME . "Elias Mårtenson") (:TEXT . "<p>3</p>")
       (:HIGHLIGHTED)))))))))
