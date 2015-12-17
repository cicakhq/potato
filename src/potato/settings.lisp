(in-package :potato.settings)

(declaim #.potato.common::*compile-decl*)

(defun show-settings-screen (extra-fields)
  ;; We reload the user here since this function can be called
  ;; immediately after upadting a user
  (let* ((user (potato.core:load-user (potato.core:user/id (potato.core:current-user))))
         (data `((:api-token . ,(potato.core:user/api-token user))
                 (:image-assigned-p . ,(string/= (potato.core:user/image-name user) ""))
                 (:image-url . ,(potato.user-image:image-url-for-user user))
                 (:description . ,(potato.core:user/description user))
                 ,@extra-fields)))
    (log:trace "Showing settings: ~s" data)
    (lofn:show-template-stream "settings.tmpl" data)))

(potato.core:define-handler-fn-login (settings-screen "/settings" nil ())
  (potato.core:with-authenticated-user ()
    (lofn:case-method
      (:get (show-settings-screen nil))
      (:post (lofn:with-parameters (description password1 password2)
               (let* ((loaded-user (potato.core:load-user (potato.core:ensure-user-id (potato.core:current-user))))
                      (response nil)
                      (updated nil))
                 (cond ((string= description "")
                        (push '(:description-error . "Name can't be blank") response))
                       (t
                        (setf (potato.core:user/description loaded-user) description)
                        (setq updated t)))
                 (cond
                   ((not (string= password1 password2))
                    (push '(:password-error . "Password do not match") response))
                   ((plusp (length password1))
                    (potato.core:user/update-password loaded-user password1)
                    (setq updated t)
                    (push '(:password-message . "Password changed") response)))
                 (when (potato.core:user/new-login loaded-user)
                   (setf (potato.core:user/new-login loaded-user) nil)
                   (setq updated t))
                 (when updated
                   (potato.core:save-user loaded-user))
                 (show-settings-screen response)))))))
