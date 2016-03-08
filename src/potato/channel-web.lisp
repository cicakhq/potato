(defpackage :potato.web.channel
  (:use :cl :potato :potato.common :potato.web))

(in-package :potato.web.channel)

(declaim #.potato.common::*compile-decl*)

(defparameter *max-history-length* 200
  "The maximum number of messages returned from a call to /history")

(defun show-channel-screen (channel-id url dev)
  (potato.core:with-authenticated-user ()
    (let* ((user (potato.core:current-user))
           (channel (potato.core:load-channel-with-check channel-id :if-not-joined :join))
           (group (potato.core:load-group-with-check (potato.core:channel/group channel)))
           (domain (potato.core:load-domain-with-check (potato.core:group/domain group) user))
           (domain-tree (potato.web:make-group-channel-tree-for-user-and-domain domain user)))
      ;; When loading the main channel page, clear all outstanding
      ;; notifications for that channel. We can simply assume that the
      ;; user will catch up on all new messages when loading the page.
      (potato.user-notification:mark-notifications-for-user-channel user channel)
      ;;
      (lofn:show-template-stream "channel.tmpl"
                                 `((:debug            . ,*debug*)
                                   (:channel-id       . ,(potato.core:channel/id channel))
                                   (:channel-name     . ,(potato.core:channel/name channel))
                                   (:channel-url      . ,url)
                                   (:user-id          . ,(potato.core:user/id user))
                                   (:email            . ,(potato.core:user/primary-email user))
                                   (:user-description      . ,(potato.core:user/description user))
                                   (:group            . ,(potato.core:group/id group))
                                   (:group-name       . ,(potato.core:group/name group))
                                   (:domain-id        . ,(potato.core:group/domain group))
                                   (:domain-name      . ,(potato.core:domain/name domain))
                                   (:admin-p          . ,(potato.core:user-is-admin-in-group-p group user))
                                   (:group-type       . ,(potato.core:group/type group))
                                   (:domain-tree      . ,domain-tree)
                                   (:current-build-id . ,potato:*build-id*)
                                   (:s3-access-key    . ,*s3-browser-access-key*)
                                   (:s3-endpoint      . ,*s3-endpoint*)
                                   (:s3-bucket        . ,*s3-bucket*)
                                   (:websocket-url    . ,*external-websocket-listen-address*)
                                   (:dev              . ,dev)
                                   (:default-upload-location . ,potato.upload:*default-upload-location*))))))

(defmacro %define-channel-screen (name url dev)
  `(potato.core:define-handler-fn-login (,name ,(concatenate 'string url "/([a-z0-9]+)") t (channel-id))
     (show-channel-screen channel-id ,url ,dev)))

(%define-channel-screen channel-screen "/channel" nil)
(%define-channel-screen channel-dev-screen "/channel-dev" t)

(potato.core:define-handler-fn-login (channel-by-domain-and-nickname-screen "/d/([^/]+)/([^/]+)" t
                                                                            (domain-nickname channel-nickname))
  (handler-case
      (let ((cid (potato.core:find-channel-id-for-nicknames domain-nickname channel-nickname)))
        (show-channel-screen cid "/channel" nil))
    (clouchdb:document-missing () (potato.core:raise-not-found-error "No such channel"))))

(potato.core:define-json-handler-fn-login (history-screen "/history" data nil ())
  (potato.core:with-authenticated-user ()
    (let ((channel (potato.core:load-channel-with-check (st-json:getjso "channel" data)))
          (num-messages (min (or (st-json:getjso "num_messages" data) *max-history-length*) *max-history-length*))
          (start (st-json:getjso "start" data)))
      (log:trace "Loading history for channel: ~s, num-messages: ~s, start: ~s"
                 (potato.core:channel/id channel) num-messages start)
      (let ((messages (potato.core:load-message-log channel num-messages (if (eq start :null) nil start))))
        (st-json:jso "messages" (mapcar (lambda (v)
                                          (potato.core:message-cd->json v :html (potato.core:current-user)))
                                        messages))))))

(potato.core:define-json-handler-fn-login (load-message-range-screen "/range" data nil ())
  (potato.core:with-authenticated-user ()
    (let* ((message-id (st-json:getjso "message" data))
           (num-rows (or (st-json:getjso "rows" data) 5)))
      (unless (< 1 num-rows 50)
        (potato.core:raise-web-parameter-error "rows parameter out of range"))
      (let ((range (potato.core:load-message-range-with-check message-id num-rows)))
        (st-json:jso "messages" (mapcar (lambda (v)
                                          (potato.core:message-cd->json v :html (potato.core:current-user)))
                                        range))))))

(defun try-send-channel (data text)
  (json-bind ((cid "channel")) data
    (potato.workflow:send-message-to-channel (potato.core:current-user)
                                             (potato.core:load-channel-with-check cid)
                                             text)))

(potato.core:define-json-handler-fn-login (send-chat-screen "/send_chat" data nil ())
  (potato.core:with-authenticated-user ()
    (let* ((text (string-trim +BLANK-CHARS+ (st-json:getjso "text" data)))
           (length (length text)))
      (cond ((> length potato.core:*max-message-size*)
             (st-json:jso "result" "error"
                          "message" "Message too large"))
            ((plusp length)
             (alexandria:if-let ((result (try-send-channel data text)))
               ;; Successfully saved the message
               (st-json:jso "result" "ok"
                            "id" (cdr (assoc :|id| result)))
               ;; ELSE: Failed to save message
               (error "Illegal data format")))
            (t
             (st-json:jso "result" "error"
                          "message" "Message was empty"))))))

(defun check-message-modification-permission (message)
  (let ((user (potato.core:current-user))
        (channel (potato.core:load-channel-with-check (potato.core:message/channel message))))
    (unless (or (equal (potato.core:user/id user) (potato.core:message/from message))
                (potato.core:user-is-admin-in-group-p (potato.core:channel/group channel) user))
      (potato.core:raise-permission-error "Update of message is not allowed"))))

(defun update-or-delete-message (message-id &key
                                              (text nil text-p) (extra-html nil extra-html-p)
                                              (image nil image-p) (deleted nil deleted-update))
  (unless (or text-p extra-html-p image-p deleted-update)
    (error "One of :text :extra-html :image or :deleted needs to be specified"))
  (let* ((m (potato.core::load-message message-id)))
    (check-message-modification-permission m)
    (potato.core:save-message-modification m (potato.core:current-user)
                                           (if text-p text (potato.core:message/text m))
                                           (if extra-html-p extra-html (potato.core:message/extra-html m))
                                           (if image-p image (potato.core:message/image m))
                                           (if deleted-update deleted (potato.core:message/deleted m)))))

(potato.core:define-json-handler-fn-login (update-chat-screen "/update_chat" data nil ())
  (potato.core:with-authenticated-user ()
    (lofn:case-method
      (:post
       (json-bind ((message-id "message") (text "text")) data
         (let ((length (length text)))
           (cond ((> length potato.core:*max-message-size*)
                  (st-json:jso "result" "error"
                               "message" "Message too large"))
                 (t
                  (update-or-delete-message message-id :text text)
                  (st-json:jso "result" "ok")))))))))

(potato.core:define-json-handler-fn-login (delete-chat-screen "/delete_chat" data nil ())
  (potato.core:with-authenticated-user ()
    (lofn:case-method
      (:post
       (let ((message-id (st-json:getjso "message" data)))
         (update-or-delete-message message-id :deleted t)
         (st-json:jso "result" "ok"))))))

(potato.core:define-json-handler-fn-login (load-message-screen "/load_chat" data nil ())
  (potato.core:with-authenticated-user ()
    (lofn:case-method
      (:post
       (let* ((message-id (st-json:getjso "message" data))
              (type (st-json:getjso "type" data))
              (message (potato.core:load-message-with-check message-id (potato.core:current-user))))
         (potato.core:message-detailed->json message (if type
                                                         (string-case:string-case (type)
                                                           ("html" :html)
                                                           ("json" :alist)
                                                           ("text" :text))
                                                         :html)
                                             (potato.core:current-user)))))))

(defun channel-users->json (channel channel-users)
  "Given a CHANNEL-USERS instance, return a JSON formatted version of the content."
  (check-type channel potato.core:channel)
  (check-type channel-users potato.core:channel-users)
  (let ((user-data (getfield (intern (potato.core:user/id (potato.core:current-user)) "KEYWORD")
                             (potato.core:channel-users/users channel-users)))
        (name (if (eq (potato.core:channel-users/group-type channel-users) :private)
                  (potato.core:user/description (potato.core:ensure-user (potato.private:find-chat-counterpart channel-users (potato.core:current-user))))
                  (potato.core:channel/name channel))))
    (st-json:jso "id" (potato.core:channel/id channel)
                 "name" name
                 "topic" (potato.core:channel/topic channel)
                 "hide" (st-json:as-json-bool (getfield :|hide| user-data :accept-missing t))
                 "group" (potato.core:channel/group channel)
                 "group_type" (symbol-name (potato.core:channel-users/group-type channel-users))
                 "unread_count" (or (getfield :|count| user-data :accept-missing t) 0))))

(potato.core:define-json-handler-fn-login (load-channels-for-user-screen "/load_channels_for_user" data nil ())
  "Return a list of all channels accessible to the current user for the given domain."
  (potato.core:with-authenticated-user ()
    (let* ((user (potato.core:current-user))
           (domain (potato.core:load-domain-with-check (st-json:getjso "domain" data) user))
           (result (clouchdb:invoke-view "user" "channels_by_domain_and_user"
                                         :key (list (potato.core:user/id user)
                                                    (potato.core:domain/id domain))))
           (rows (getfield :|rows| result))
           (counterparty-ids (mapcan (lambda (v)
                                       (let ((counterpart (nth 6 (getfield :|value| v))))
                                         (when counterpart
                                           (list counterpart))))
                                     rows))
           (counterparty-names (mapcar #'cons counterparty-ids
                                       (potato.core:find-descriptions-for-users counterparty-ids))))
      (st-json:jso "channels" (mapcar (lambda (v)
                                        (destructuring-bind (cid name hide group group-type unread-count cpt-id)
                                            (getfield :|value| v)
                                          (st-json:jso "id" cid
                                                       "name" (if cpt-id
                                                                  (or (cdr (assoc cpt-id counterparty-names
                                                                                  :test #'string=))
                                                                      (error "No cpt name"))
                                                                  name)
                                                       "hide" (st-json:as-json-bool hide)
                                                       "group" group
                                                       "group_type" group-type
                                                       "unread_count" unread-count )))
                                      rows)))))

(potato.core:define-json-handler-fn-login (load-channel-screen "/load_channel" data nil ())
  (potato.core:with-authenticated-user ()
    (multiple-value-bind (channel channel-users)
        (potato.core:load-channel-with-check (st-json:getjso "channel" data))
      (st-json:jso "channel" (channel-users->json channel channel-users)))))

(potato.core:define-json-handler-fn-login (update-star-screen "/update_star" data nil ())
  (potato.core:with-authenticated-user ()
    (let ((message-id (st-json:getjso "message" data))
          (enable-p (st-json:from-json-bool (st-json:getjso "enable" data))))
      (log:trace "Updating star for message: ~s, user: ~s, enable: ~s"
                 message-id (potato.core:user/id (potato.core:current-user)) enable-p)
      (potato.core:update-message-star-with-check message-id enable-p)
      (st-json:jso "result" "ok"))))

(potato.core:define-json-handler-fn-login (update-hidden-screen "/update_hidden" data nil ())
  (potato.core:with-authenticated-user ()
    (let ((message-id (st-json:getjso "message" data))
          (enable-p (st-json:from-json-bool (st-json:getjso "enable" data))))
      (log:trace "Updating hidden for message: ~s, user: ~s, enable: ~s"
                 message-id (potato.core:user/id (potato.core:current-user)) enable-p)
      (potato.core:update-message-hidden-with-check message-id enable-p)
      (st-json:jso "result" "ok"))))

(potato.core:define-handler-fn-login (create-channel-screen "/createchannel" nil ())
  (potato.core:with-authenticated-user ()
    (lofn:case-method
      (:post (lofn:with-checked-parameters ((group-id :name "group" :required t :allow-blank nil :trimmed t)
                                            (channel-name :name "name" :required t :allow-blank nil :trimmed t)
                                            (channel-topic :name "topic" :required nil :trimmed t))
               (let ((channel (potato.workflow:create-channel-with-check (potato.core:current-user)
                                                                         group-id channel-name channel-topic)))
                 (hunchentoot:redirect (format nil "/channel/~a" (potato.core:channel/id channel)))))))))
