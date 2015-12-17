(in-package :potato.sso)

(defvar *keytab-file* #p"potato.keytab")
(defvar *ldap-hostname* "localhost")
(defvar *ldap-port* 389)
(defvar *ldap-user* "demo")
(defvar *ldap-password* "demo")
(defvar *ldap-base* "dc=company,dc=com")

(defclass ldap-user ()
  ((name        :type string
                :initarg :name
                :initform (error "~s needed for ~s" :name 'ldap-user)
                :reader ldap-user/name)
   (description :type string
                :initarg :description
                :initform (error "~s needed for ~s" :description 'ldap-user)
                :reader ldap-user/description)
   (email       :type string
                :initarg :email
                :initform (error "~s needed for ~s" :email 'ldap-user)
                :reader ldap-user/email)
   (dn          :type string
                :initarg :ldap-dn
                :initform (error "~s needed for ~s" :dn 'ldap-user)
                :reader ldap-user/ldap-dn)))

(defmethod print-object ((obj ldap-user) stream)
  (print-unreadable-safely (name description dn email) obj stream
    (format stream "NAME ~s DESCRIPTION ~s DN ~s EMAIL ~s" name description dn email)))

(defun connect-ldap (user password)
  (ldap:new-ldap :host *ldap-hostname*
                 :port *ldap-port*
                 :base *ldap-base*
                 :user user
                 :pass password))

(defmacro with-connect-ldap ((ldap-sym) &body body)
  (let ((sym (gensym)))
    `(let ((,sym (connect-ldap *ldap-user* *ldap-password*)))
       (unwind-protect
            (progn
              (ldap:bind ,sym)
              (let ((,ldap-sym ,sym))
                ,@body))
         (ldap:unbind ,sym)))))

(defun make-query-for-username (username-input)
  (if (position #\@ username-input)
      (when (cl-ppcre:scan "(?i)^([a-z._-]+)@murex\\.com$" username-input)
        (format nil "(&(sAMAccountName=*)(mail=~a))" username-input))
      ;; ELSE: Username does not contain a @, assume corporate user
      (progn
        ;; Avoid injection attacks
        (unless (cl-ppcre:scan "^[a-zA-Z0-9_-]+$" username-input)
          (potato.core:raise-permission-error "Illegal username"))
        (format nil "(sAMAccountName=~a)" username-input))))

(defun raise-username-or-password-error (name)
  (log:trace "Login failed for user: ~s" name)
  (potato.core:raise-permission-error "Incorrect username or password"))

(defun lookup-user (name)
  (let ((query (make-query-for-username name)))
    (when query
      (with-connect-ldap (ldap)
        (when (ldap:search ldap query
                           :attributes '(:sAMAccountName
                                         :displayName
                                         :mail))
          (let ((res (ldap:next-search-result ldap)))
            (when res
              (let ((user (make-instance 'ldap-user
                                         :name (car (ldap:attr-value res :sAMAccountName))
                                         :description (car (ldap:attr-value res :displayName))
                                         :email (car (ldap:attr-value res :mail))
                                         :ldap-dn (ldap:dn res))))
                (when (ldap:next-search-result ldap)
                  (raise-username-or-password-error name))
                user))))))))

(defun sso-init (parameters)
  (macrolet ((with-parameter ((var keyword required) &body body)
               (let ((var-sym (gensym)))
                 `(let ((,var-sym (assoc ,keyword parameters)))
                    (if ,var-sym
                        (let ((,var (cdr ,var-sym)))
                          ,@body)
                        ,@(if required
                              `((error ,(format nil "Missing config option: ~s" keyword)))))))))
    (with-parameter (keytab-file :keytab-file t)
      (cl-gss:krb5-register-acceptor-identity keytab-file))
    (with-parameter (ldap-hostname :ldap-hostname t)
      (setq *ldap-hostname* ldap-hostname))
    (with-parameter (ldap-port :ldap-port nil)
      (unless (integerp ldap-port)
        (error "LDAP port setting is not an integer"))
      (setq *ldap-port* ldap-port))
    (with-parameter (ldap-user :ldap-user t)
      (setq *ldap-user* ldap-user))
    (with-parameter (ldap-password :ldap-password t)
      (setq *ldap-password* ldap-password))
    (with-parameter (ldap-base :ldap-base t)
      (setq *ldap-base* ldap-base))
    (setq potato.core:*authenticator-function* #'sso-authenticate)
    (setq potato.core:*external-user-check-function* #'ldap-check-user)))

(defun convert-krb-name-to-email (name)
  (multiple-value-bind (match strings)
      (cl-ppcre:scan-to-strings "^([a-z0-9_-]+)@[A-Z]+\.MUREX\.COM$" (cl-gss:name-to-string name))
    (when match
      (alexandria:when-let ((user (lookup-user (aref strings 0))))
        (list (string-downcase (ldap-user/email user)) (ldap-user/description user))))))

(defun sso-authenticate (success-handler-fn failure-handler-fn)
  (labels ((failed-auth ()
             (setf (hunchentoot:return-code*) hunchentoot:+http-authorization-required+)
             (or (funcall failure-handler-fn)
                 "Authentication needed")))
    (handler-case
        (let ((auth (hunchentoot:header-in* :authorization))
              #+nil(session (potato.core:find-current-user-session)))
          (if auth
              ;; The client supplied the 'Authorization' header
              (let ((parts (split-sequence:split-sequence #\Space auth)))
                (unless (and (= (length parts) 2)
                             (string= (string-downcase (car parts)) "negotiate"))
                  (error "Can't parse result of 'Authorization' header"))
                (let ((buf (cl-base64:base64-string-to-usb8-array (cadr parts)))
                      (context #+nil(potato.core:session-value session 'spnego-context)))
                  (multiple-value-bind (continue-needed context-ret name buffer)
                      (cl-gss:accept-sec buf :context context)
                    #-nil (declare (ignore context-ret))
                    #+nil(unless context
                           (setf (potato.core:session-value session 'spnego-context) context-ret))
                    (when buffer
                      (setf (hunchentoot:header-out "WWW-Authenticate")
                            (format nil "Negotiate ~a"
                                    (cl-base64:usb8-array-to-base64-string buffer))))
                    (cond (continue-needed
                           (failed-auth))
                          (t
                           #+nil(setf (potato.core:session-value session 'spnego-context) nil)
                           (let ((translated-name (convert-krb-name-to-email name)))
                             (if translated-name
                                 (funcall success-handler-fn translated-name)
                                 (failed-auth))))))))
              ;; ELSE The client did not supply the 'Authorization' header
              (progn
                (setf (hunchentoot:header-out "WWW-Authenticate") "Negotiate")
                (failed-auth))))
      (cl-gss:gss-error (condition)
        (log:error "Error during initialisation of GSS context error: ~a" condition)
        (failed-auth)))))

(defun ldap-check-user (username-input password)
  (flet ((fail ()
           (raise-username-or-password-error username-input)))
    (if (zerop (length password))
        ;; Active Directory allows bind with blank password
        (fail)
        ;; ELSE: We have a password, try to do a bind
        (alexandria:if-let ((ldap-user (lookup-user username-input)))
          (let ((ldap (connect-ldap (ldap-user/ldap-dn ldap-user) password)))
            (unwind-protect
                 (if (ldap:bind ldap)
                     (list (string-downcase (ldap-user/email ldap-user)) (ldap-user/description ldap-user))
                     ;; ELSE: Incorrect password
                     (fail))
              (ldap:unbind ldap)))
          ;; ELSE: User does not exist
          (fail)))))
