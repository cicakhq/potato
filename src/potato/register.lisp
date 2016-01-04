(in-package :potato.core)

(declaim #.potato.common::*compile-decl*)

(defvar *inhibit-no-smtp-server-warning* nil)

(defun show-register-template (args)
  (lofn:with-parameters (m (redirect-path "redirect_path"))
    (lofn:show-template-stream "register.tmpl" (append args
                                                       (list (cons :activate-api (equal m "1"))
                                                             (cons :redirect-path redirect-path))))))

(defun show-email-conflict-error (email description enable-api redirect-path)
  (show-register-template (list (cons :email-error "Email address is already registered")
                                (cons :email email)
                                (cons :description description)
                                (cons :activate-api enable-api)
                                (cons :redirect-path redirect-path))))

(defun make-unregistered-user (description password)
  (let* ((id (potato.db:make-random-couchdb-id))
         (user (make-instance 'user
                              :couchdb-id (concatenate 'string "user-" id)
                              :nickname id
                              :description description
                              :password (or password "")
                              :activated-p nil
                              :activate-code (ironclad:byte-array-to-hex-string (secure-random:bytes 16 secure-random:*generator*))
                              :default-image-name (format nil "~a.png" (1+ (random 7))))))
    (when password
      (potato.core::user/update-password user password))
    user))

(defun register-user-and-redirect (email description password enable-api redirect-path)
  (when (load-user-by-email email :error-if-not-found nil)
    (show-email-conflict-error email description enable-api redirect-path)
    (return-from register-user-and-redirect))

  (handler-case
      (let ((user (potato.workflow:register-user email description password enable-api nil)))
        (send-activation-email user)
        (if redirect-path
            (hunchentoot:redirect (format nil "~a?api-key=~a" redirect-path (user/api-token user)))
            (hunchentoot:redirect "/")))
    (clouchdb:id-or-revision-conflict ()
      (show-email-conflict-error email description enable-api redirect-path))))

(defun handle-register-user ()
  (lofn:with-parameters (email description password1 activate-api redirect-path)
    (let ((email-trimmed (string-downcase (potato.core:trim-string email)))
          (description-trimmed (potato.core:trim-string description))
          (enable-api (string= activate-api "1"))
          (errors nil))
      (unless (is-allowed-email-p email-trimmed)
        (push (cons :email-error "Illegal email address") errors))
      (cond ((< (length password1) 1)
             (push (cons :password-error "Password is too short") errors)))
      (when (string= description-trimmed "")
        (push (cons :description-error "Name can't be blank") errors))
      ;; If errors, send back to the registration page
      (if errors
          (show-register-template errors)
          (register-user-and-redirect email description password1 enable-api redirect-path)))))

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
    (unless *smtp-server-host*
      (unless *inhibit-no-smtp-server-warning*
        (log:warn "No SMTP server specified. Registration email will not be sent. Manual registration URL: ~a" url))
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
  (lofn:with-parameters ((user-id "user") (activate-code "id"))
    (let ((user (load-user (decode-name user-id))))
      (cond ((user/activated-p user)
             "User is already activated")
            ((string= (user/activate-code user) activate-code)
             (activate-user user)
             (lofn:show-template-stream "user_activated_notification.tmpl" nil))
            (t
             "Illegal activation code")))))

(define-handler-fn-login (send-activation-email-screen "/send_activation_email" nil ())
  (with-authenticated-user (t)
    (let ((user (current-user)))
      (unless (user/activated-p user)
        (send-activation-email user))
      (lofn:show-template-stream "activation_email_sent.tmpl" `((:user-activated-p . ,(user/activated-p user)))))))
