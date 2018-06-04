(in-package :potato.core)

(declaim #.potato.common::*compile-decl*)

(defvar *inhibit-no-smtp-server-warning* nil)

(defun show-register-template (args)
  (lofn:with-checked-parameters ((mobile-p :name "m" :required nil :type :boolean)
                                 (redirect-path :name "redirect_path" :required nil))
    (lofn:show-template-stream "register.tmpl" (append args
                                                       (list (cons :activate-api mobile-p)
                                                             (cons :redirect-path redirect-path))))))

(defun show-email-conflict-error (email description enable-api redirect-path)
  (show-register-template (list (cons :email-error "Email address is already registered")
                                (cons :email email)
                                (cons :description description)
                                (cons :activate-api enable-api)
                                (cons :redirect-path redirect-path))))

(defun make-unregistered-user (description password)
  (let* ((user (make-instance 'user
                              :nickname (potato.db:make-random-couchdb-id)
                              :description description
                              :password (or password "")
                              :activated-p nil
                              :activate-code (ironclad:byte-array-to-hex-string (secure-random:bytes 16 secure-random:*generator*))
                              :default-image-name (format nil "~a.png" (1+ (secure-random:number 7))))))
    (when password
      (potato.core::user/update-password user password))
    user))

(defun register-user-and-redirect (email description password enable-api redirect-path)
  (let ((loaded-user (load-user-by-email email :error-if-not-found nil)))
    (when loaded-user
      (when (potato.core:user/activated-p loaded-user)
        (show-email-conflict-error email description enable-api redirect-path)
        (return-from register-user-and-redirect))
      ;; There is already a user linked to this email, but the user
      ;; has not been activated. In this case we can simply delete
      ;; that link. Keep the user in order to keep the database
      ;; consistency intact.
      (let ((useremail (potato.core:load-user-email-by-email email)))
        (potato.db:remove-instance useremail))))

  (handler-case
      (let ((user (potato.workflow:register-user email description password enable-api nil)))
        (send-activation-email user)
        (hunchentoot:redirect (cond ((null redirect-path)
                                     "/activate")
                                    (enable-api
                                     (format nil "~a?email=~a&api-key=~a"
                                             redirect-path
                                             (user/primary-email user)
                                             (user/api-token user)))
                                    (t
                                     redirect-path))))
    (clouchdb:id-or-revision-conflict ()
      (show-email-conflict-error email description enable-api redirect-path))))

(defun handle-register-user ()
  (lofn:with-checked-parameters ((email :required t :trimmed t)
                                 (description :required t :trimmed t)
                                 (password1 :name "password1" :required t)
                                 (password2 :name "password2" :required t)
                                 (mobile-p :name "m" :type :boolean))
    (let ((email-trimmed (string-downcase email))
          (errors nil))
      (unless (is-allowed-email-p email-trimmed)
        (push (cons :email-error "Illegal email address") errors))
      (cond ((not (equal password1 password2))
             (push (cons :password-error "Passwords does not match") errors))
            ((< (length password1) 1)
             (push (cons :password-error "Password is too short") errors)))
      (when (string= description "")
        (push (cons :description-error "Name can't be blank") errors))
      ;; If errors, send back to the registration page
      (if errors
          (show-register-template errors)
          (register-user-and-redirect email-trimmed description password1 mobile-p
                                      (if mobile-p "potato://sent-registration"))))))

(lofn:define-handler-fn (register-screen "/register" nil ())
  (when *authenticator-function*
    (error "Can't register users when using external authenticator"))
  (lofn:case-method
    (:get (show-register-template nil))
    (:post (handle-register-user))))

(lofn:define-handler-fn (register-mobile-screen "/mobile_register" nil ())
  (lofn:case-method
    (:get (show-register-template '((:activate-api . t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Activation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun send-activation-email (user)
  (when (user/activated-p user)
    (log:warn "Attempt to activate a user which has already been activated: ~a" (user/id user))
    (return-from send-activation-email))

  (let ((url (potato.core:make-potato-url "activate?user=~a&id=~a"
                                          (encode-name (user/id user)) (user/activate-code user))))
    (unless *email-type*
      (unless *inhibit-no-smtp-server-warning*
        (log:warn "No email sender configured. Registration email will not be sent. Manual registration URL: ~a" url))
      (return-from send-activation-email))
    (potato.email:send-email (make-instance 'potato.email:mail-descriptor
                                            :to-name (user/description user)
                                            :to-email (user/primary-email user)
                                            :subject "Please activate your Potato account"
                                            :text-content (format nil "In order to activate your account, follow the following link: ~a" url)))))

(defun activate-user (user)
  (setf (user/activated-p user) (format-timestamp nil (local-time:now)))
  (setf (user/activate-code user) "")
  (save-user user))

(lofn:define-handler-fn (activate-screen "/activate" nil ())
  (lofn:with-checked-parameters ((user-id :name "user")
                                 (activate-code :name "id"))
    (if (and user-id activate-code)
        (let ((user (load-user (decode-name user-id))))
          (cond ((user/activated-p user)
                 "User is already activated")
                ((string= (user/activate-code user) activate-code)
                 (activate-user user)
                 (lofn:show-template-stream "user_activated_notification.tmpl" nil))
                (t
                 "Illegal activation code")))
        ;; ELSE: Display a message telling the user to wait for the email
        (lofn:show-template-stream "activation_helper.tmpl" nil))))

(define-handler-fn-login (send-activation-email-screen "/send_activation_email" nil ())
  (with-authenticated-user (t)
    (let ((user (current-user)))
      (unless (user/activated-p user)
        (send-activation-email user))
      (lofn:show-template-stream "activation_email_sent.tmpl" `((:user-activated-p . ,(user/activated-p user)))))))

(define-handler-fn-login (user-not-activated-screen "/user_not_activated" nil ())
  (with-authenticated-user (t)
    (let ((user (current-user)))
      (lofn:show-template-stream "user_not_activated.tmpl" `((:user-activated-p . ,(user/activated-p user)))))))
