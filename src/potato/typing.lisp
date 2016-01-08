(in-package :potato.core)

(declaim #.potato.common::*compile-decl*)

(defun send-typing-end-notification-to-state-server (channel user)
  (let ((channel-id (ensure-channel-id channel))
        (user-id (ensure-user-id user)))
    (send-rabbitmq-message-to-state-server channel-id nil (list *state-server-message-typing-end*
                                                                channel-id user-id))))

(defun send-typing-start-notification-to-state-server (channel user)
  (let ((cid (ensure-channel-id channel))
        (uid (ensure-user-id user)))
    (log:trace "Received type notification for user=~s, channel=~s" uid cid)
    (send-rabbitmq-message-to-state-server cid nil (list *state-server-message-typing-start* cid uid))))

(potato.core:define-json-handler-fn-login (type-notification-screen "/type_notification/([^/]+)/([^/]+)"
                                                                    data t (domain-id channel-id))
  (declare (ignore data))
  (with-authenticated-user ()
    (let ((channel (load-channel-with-check channel-id)))
      (unless (equal (channel/domain channel) domain-id)
        (error "Illegal domain specified"))
      (send-typing-start-notification-to-state-server channel (current-user))
      (st-json:jso "result" "ok"))))
