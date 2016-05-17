(in-package :potato.register)

(declaim #.potato.common::*compile-decl*)

(defvar *inhibit-no-smtp-server-warning* nil)
(defvar *login-request-expiry* 5
  "Number of minutes that the login request is valid for")

(defclass login-request ()
  ((email          :type string
                   :initarg :email
                   :initform (error "~s required for ~s" :email 'login-request)
                   :reader login-request/email
                   :persisted-p t)
   (created-date   :type local-time:timestamp
                   :initform (local-time:now)
                   :reader login-request/created-date
                   :persisted-p t
                   :persisted-type :date)
   (key            :type string
                   :initform (potato.core::make-random-name 20)
                   :reader login-request/key
                   :persisted-p t)
   (user           :type (or null string)
                   :initarg :user
                   :initform (error "~s required for ~s" :user 'login-request)
                   :reader login-request/user
                   :persisted-p t)
   (save           :type (or null (eql t))
                   :initarg :save-login
                   :initform nil
                   :reader login-request/save-login
                   :persisted-p t
                   :persisted-type :boolean)
   (password-reset :type (or null (eql t))
                   :initarg :password-reset
                   :reader login-request/password-reset
                   :persisted-p t
                   :persisted-type :boolean))
  (:metaclass potato.db:persisted-entry-class))

(defmethod print-object ((obj login-request) stream)
  (print-unreadable-safely (email user save) obj stream
    (format stream "EMAIL ~s USER ~s SAVE ~s" email user save)))

(defun make-description-from-email (email)
  "Given an email address, create a default user description based on
it. Currently, this function simply returns the username part of the
email address."
  (let ((pos (position #\@ email)))
    (if pos
        (subseq email 0 pos)
        email)))

(defun make-login-request-id (email)
  (concatenate 'string "loginrequest-" (encode-name email)))

(defun send-login-email (email-address url key)
  (let ((mail (make-instance 'potato.email:mail-descriptor
                             :to-email email-address
                             :subject "Please log in to Potato"
                             :text-content (format nil "Please log in using key: ~a, or by following this link: ~a" key url)
                             :html-content (format nil "Please log in using key: ~a, or by following this <a href=\"~a\">link</a>" key url))))
    (potato.email:send-email mail)))

(defun register2-post-handler (email password-reset-p)
  (let ((email (string-downcase email)))
    (let ((id (make-login-request-id email)))
      (clouchdb:delete-document id :if-missing :ignore)
      (let ((user (potato.core:load-user-by-email email :error-if-not-found nil)))
        (cond ((and (null user)
                    password-reset-p)
               (log:info "Attempt to recover password for nonexistent user: ~s" email))

              ((and (not *allow-password-recovery*)
                    user
                    (not (equal (user/password user) "")))
               (error "User has a password"))

              (t
               (let ((req (make-instance 'login-request
                                         :couchdb-id id
                                         :email email
                                         :user (if user (user/id user) nil)
                                         :save-login t
                                         :password-reset password-reset-p)))
                 (potato.db:save-instance req)
                 (let ((url (potato.core:make-potato-url "register2_accept?email=~a&code=~a"
                                                         (encode-name email) (login-request/key req))))
                   (if *smtp-server-host*
                       (send-login-email email url (login-request/key req))
                       (log:warn "No SMTP server specified, login URL: ~a" url))))))
        ;; Always display the authentication page in order to avoid information leakage
        (lofn:show-template-stream "register2_loginkey.tmpl" `((:email . ,email)))))))

(lofn:define-handler-fn (register2-screen "/register2" nil ())
  (unless *allow-passwordless-login*
    (raise-not-found-error "Illegal registration mode"))
  (lofn:case-method
    (:get (lofn:show-template-stream "register2.tmpl" nil))
    (:post (lofn:with-checked-parameters ((email :name "email" :required t :trimmed t :allow-blank nil))
             (register2-post-handler email nil)))))

(defun verify-login-request (email code)
  (let ((req (potato.db:load-instance 'login-request (make-login-request-id email) :error-if-not-found nil)))
    (when req
      (clouchdb:delete-document (potato.db:persisted-entry/couchdb-id req))
      (let ((timeout (local-time:timestamp+ (login-request/created-date req) *login-request-expiry* :minute)))
        (when (and (local-time:timestamp> timeout (local-time:now))
                   (string= (login-request/key req) code))
          req)))))

(defun make-registered-and-activated-user (email &key description)
  (potato.workflow:register-user email (or description (make-description-from-email email)) nil nil t))

(defun login-and-maybe-create-new-user (req)
  "Create the user if it doesn't already exist, and initialise the session.
If the user previously had a password, wipe it and return T. Otherwise return NIL."
  (alexandria:if-let ((user-id (login-request/user req)))
    (let ((user (load-user user-id)))
      (when (not (equal (user/password user) ""))
        (potato.core:clear-user-password user))
      (potato.core:setup-user-session user (login-request/save-login req))
      nil)
    ;; ELSE: Create the user
    (let ((user (make-registered-and-activated-user (login-request/email req))))
      (potato.core:setup-user-session user (login-request/save-login req)))))

(lofn:define-handler-fn (register2-accept-screen "/register2_accept" nil ())
  (lofn:with-checked-parameters (email code)
    (alexandria:if-let ((req (verify-login-request email code)))
      (progn
        (log:trace "Processing login request for: ~s" req)
        (login-and-maybe-create-new-user req)
        (if (login-request/password-reset req)
            (hunchentoot:redirect "/update-password")
            (hunchentoot:redirect "/")))
      ;; ELSE: Send back to login screen
      (hunchentoot:redirect "/register2"))))
