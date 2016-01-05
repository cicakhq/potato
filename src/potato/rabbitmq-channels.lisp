(in-package :potato.rabbitmq-channels)

(declaim #.potato.common::*compile-decl*)

;;;
;;;  Routing key format:
;;;
;;;     command.channel
;;;
;;;  Command is one of: user-name-update, channel-change, update-star
;;;

(defun process-channel-update (msg)
  (let* ((body (cl-rabbit:message/body (cl-rabbit:envelope/message msg)))
         (content (binary-to-lisp body)))
    (log:trace "Got channel update: ~s" content)
    (case (car content)
      (:user-join
       (destructuring-bind (uid description image-url)
           (cdr content)
         (st-json:jso "type" "user-join"
                      "user" uid
                      "description" description
                      "image_url" image-url)))
      (:user-name-change
       (destructuring-bind (uid description nickname image-name)
           (cdr content)
         (st-json:jso "type" "user-name-change"
                      "user" uid
                      "description" description
                      "nickname" nickname
                      "image_name" image-name)))
      (:channel-change
       (destructuring-bind (cid name topic)
           (cdr content)
         (st-json:jso "type" "channel-change"
                      "channel" cid
                      "name" name
                      "topic" topic)))
      (:update-star
       (destructuring-bind (cid msgid uid add-p)
           (cdr content)
         ;; Only send star updates to the user who updated the star
         (when (equal uid (potato.core:user/id (potato.core:current-user)))
           (st-json:jso "type" "update-star"
                        "channel" cid
                        "message" msgid
                        "add" (st-json:as-json-bool add-p)))))
      (t
       (log:warn "Unpexted channel update command: ~s" (car content))))))

(defun notify-user-changed (user)
  (let ((uid (potato.core:user/id user)))
    (with-pooled-rabbitmq-connection (conn)
      (let ((channels (potato.core:find-channels-for-user user))
            (body (lisp-to-binary (list :user-name-change
                                        uid
                                        (potato.core:user/description user)
                                        (potato.core:user/nickname user)
                                        (potato.user-image:image-url-for-user user)))))
        (dolist (cid channels)
          (cl-rabbit:basic-publish conn 1
                                   :exchange *channel-exchange-name*
                                   :routing-key (format nil "user-name-change.~a" (encode-name-for-routing-key cid))
                                   :body body))))))

(potato.db:define-hook-fn push-notification-on-user-description-change potato.core:user (user)
  (when (or (potato.db:persisted-entry-is-value-updated user 'potato.core::description)
            (potato.db:persisted-entry-is-value-updated user 'potato.core::nickname)
            (potato.db:persisted-entry-is-value-updated user 'potato.core::image-name))
    (notify-user-changed user)))

(defun notify-channel-changed (channel)
  (with-pooled-rabbitmq-connection (conn)
    (let* ((cid (potato.core:channel/id channel))
           (body (lisp-to-binary (list :channel-change
                                       cid
                                       (potato.core:channel/name channel)
                                       (potato.core:channel/topic channel)))))
      (cl-rabbit:basic-publish conn 1
                               :exchange *channel-exchange-name*
                               :routing-key (format nil "channel-change.~a" (encode-name-for-routing-key cid))
                               :body body))))

(potato.db:define-hook-fn push-notification-on-channel-change potato.core:channel (obj)
  (log:trace "Checking for updates on channel object: ~s" obj)
  (when (or (potato.db:persisted-entry-is-value-updated obj 'potato.core::name)
            (potato.db:persisted-entry-is-value-updated obj 'potato.core::topic)
            (potato.db:persisted-entry-is-value-updated obj 'potato.core::nickname))
    (notify-channel-changed obj)))
