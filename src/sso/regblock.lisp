(in-package :potato.regblock)

(defvar *permit-email-patterns* nil)

(defun validate-register-user (email description)
  (declare (ignore description))
  (let ((email-lower (string-downcase email)))
    (loop
      for validator in *permit-email-patterns*
      when (cl-ppcre:scan validator email-lower)
        return t
      finally (return nil))))

(defun regblock-init (params)
  (destructuring-bind (&key permit-email)
      params
    (setq *permit-email-patterns* (mapcar #'cl-ppcre:create-scanner permit-email)))
  (setq potato:*user-registration-validation-function* #'validate-register-user))
