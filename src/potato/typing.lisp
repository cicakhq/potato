(in-package :potato.core)

(declaim #.potato.common::*compile-decl*)

(defun send-typing-end-notification-to-state-server (channel user)
  (let ((channel-id (ensure-channel-id channel))
        (user-id (ensure-user-id user)))
    (send-rabbitmq-message-to-state-server channel-id nil (list *state-server-message-typing-end*
                                                                channel-id user-id))))

(potato.core:define-json-handler-fn-login (type-notification-screen "/type_notification/([^/]+)/([^/]+)"
                                                                    data t (domain-id channel-id))
  (declare (ignore data))
  (with-authenticated-user ()
    (let ((channel (load-channel-with-check channel-id)))
      (unless (equal (channel/domain channel) domain-id)
        (error "Illegal domain specified"))
      (log:trace "Received type notification for user=~s, channel=~s"
                 (potato.core:user/id (current-user)) (potato.core:channel/id channel))
      (send-rabbitmq-message-to-state-server channel-id nil (list *state-server-message-typing-start*
                                                                  channel-id (user/id (current-user))))
      (st-json:jso "result" "ok"))))
