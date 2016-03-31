(defpackage :potato.web.private
  (:use :cl :potato :potato.common :potato.web))

(in-package :potato.web.private)

(declaim #.potato.common::*compile-decl*)

(potato.core:define-json-handler-fn-login (start-private-chat-screen "/start_private_chat" data nil ())
  (potato.core:with-authenticated-user ()
    (lofn:case-method
      (:post
       (let ((domain (potato.db:load-instance 'potato.core:domain (st-json:getjso "domain" data)))
             (chat-user (potato.core:load-user (st-json:getjso "user" data))))
         ;; The following check is done in the call to
         ;; FIND-PRIVATE-CHANNEL-FOR-USERS as well, but let's be clear
         ;; about the intent.
         (unless (and (potato.core:user-is-in-domain-p domain (potato.core:current-user))
                      (potato.core:user-is-in-domain-p domain chat-user))
           (error "Attempt to start a private chat between users that are not members of the requested domain"))
         ;; Find the channel and redirect to it
         (let ((channel (potato.private:find-private-channel-for-users domain chat-user (potato.core:current-user))))
           (st-json:jso "result" "ok"
                        "channel" (potato.core:channel/id channel))))))))

(potato.core:define-json-handler-fn-login (private-chats-for-user "/private_chats_for_user" data nil ())
  (potato.core:with-authenticated-user ()
    (lofn:case-method
      (:post
       (let ((user (potato.core:current-user))
             (domain-id (st-json:getjso "domain" data)))
         (potato.core:check-user-in-domain domain-id user)
         (let* ((channels (potato.private:find-private-channels-for-domain-and-user domain-id user))
                (counterparts (mapcar #'(lambda (v) (potato.private:find-chat-counterpart v user)) channels))
                (name-list (potato.core:find-descriptions-for-users counterparts)))
           (st-json:jso "result" "ok"
                        "channels" (loop
                                      for channel in channels
                                      for counterpart-id in counterparts
                                      for name in name-list
                                      collect (st-json:jso "channel" (potato.core:channel-users/channel-id channel)
                                                           "group" (potato.core:channel-users/group channel)
                                                           "user_id" counterpart-id
                                                           "user_description" name)))))))))
