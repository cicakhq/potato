(in-package :potato.web)

(declaim #.potato.common::*compile-decl*)

(defvar *allow-create-domain* nil)

(defun show-template-stream-with-default-parameters (file data)
  (let* ((user (potato.core:current-user))
         (params `((:user-id          . ,(potato.core:user/id user))
                   (:user-description . ,(potato.core:user/description user))))
         (appended (append params data)))
    (log:trace "Showing template stream for ~s, data: ~s" file appended)
    (lofn:show-template-stream file appended)))

(potato.core:define-handler-fn-login (main-screen "/" nil ())
  (let ((user (potato.core:user-session/user (potato.core:find-current-user-session))))
    (lofn:show-template-stream "main.tmpl" (append (list (cons :logged-in-p (not (null user))))
                                                   (if user
                                                       `((:user-id             . ,(potato.core:user/id user))
                                                         (:user-email          . ,(potato.core:user/primary-email user))
                                                         (:user-description    . ,(potato.core:user/description user))
                                                         (:user-registered-p   . ,(potato.core:user/activated-p user))
                                                         (:domain-tree         . ,(potato.core::domain-tree-for-user-as-template-data user))
                                                         (:new-login           . ,(potato.core:user/new-login user))
                                                         (:allow-create-domain . ,*allow-create-domain*)
                                                         (:allow-passwordless-login . ,potato.register:*allow-passwordless-login*))
                                                       nil)))))

(potato.core:define-handler-fn-login (downloads-screen "/downloads" nil ())
  (let ((user (potato.core:user-session/user (potato.core:find-current-user-session))))
    (lofn:show-template-stream "downloads.tmpl" (append (list (cons :logged-in-p (not (null user))))
                                                   (if user
                                                       `((:user-id           . ,(potato.core:user/id user))
                                                         (:user-email        . ,(potato.core:user/primary-email user))
                                                         (:user-description  . ,(potato.core:user/description user))
                                                         (:user-registered-p . ,(potato.core:user/activated-p user))
                                                         (:domain-tree       . ,(potato.core::domain-tree-for-user-as-template-data user)))
                                                       nil)))))

(defun load-channel-list (group-id)
  (loop
     for channel in (potato.core:find-channels-for-group group-id)
     collect `((:channel-id   . ,(potato.core:channel/id channel))
               (:channel-name . ,(potato.core:channel/name channel)))))

(defun make-group-channel-tree-for-user-and-domain (domain user &key (include-private t))
  (let* ((domain-id (potato.core:ensure-domain-id domain))
         (uid (potato.core:user/id user))
         (result (clouchdb:invoke-view "group" "groups_for_user"
                                       :start-key (list domain-id uid nil)
                                       :end-key (list domain-id uid 'clouchdb:json-map))))
    (loop
      for row in (getfield :|rows| result)
      for value = (getfield :|value| row)
      for group-id = (getfield :|group| value)
      when (or include-private (not (equal (getfield :|role| value) potato.core:+GROUP-USER-TYPE-PRIVATE+)))
        collect `((:group-id   . ,group-id)
                  (:group-name . ,(getfield :|group_name| value))
                  (:channels   . ,(load-channel-list group-id))))))

(potato.core:define-json-handler-fn-login (hide-channel-screen "/update_channel" data nil ())
  (potato.core:with-authenticated-user ()
    (let ((user (potato.core:current-user))
          (channel (potato.core:load-channel-with-check (st-json:getjso "channel" data)))
          (show (st-json:from-json-bool (st-json:getjso "show" data))))
      ;; Set the channel visibility
      (potato.core:update-channel-visibility-for-user channel (potato.core:current-user) show)
      ;; If the user chooses to hide a channel that has unread
      ;; messages, then clear all the messages and notifications.
      (unless show
        (potato.user-notification:mark-notifications-for-user-channel user channel)
        (potato.core:clear-unread-state-for-user-in-channel user channel)))))

(potato.core:define-json-handler-fn-login (remove-channel-screen "/remove_channel" data nil ())
  (potato.core:with-authenticated-user ()
    (let ((user (potato.core:current-user))
          (channel (potato.core:load-channel-with-check (st-json:getjso "channel" data))))
      ;; Clear outstanding notifications for the user on the channel
      (potato.user-notification:mark-notifications-for-user-channel user channel)
      ;; Remove the user from the channel
      (potato.core:remove-user-from-channel channel user))))

(potato.core:define-handler-fn-login (redirect-screen "/redirect" nil ())
  (lofn:with-checked-parameters ((location :required t :allow-blank nil))
    (lofn:show-template-stream "redirect.tmpl" (list (cons :redirect-location location)))))
