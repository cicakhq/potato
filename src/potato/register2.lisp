(in-package :potato.register)

(declaim #.potato.common::*compile-decl*)

(defvar *inhibit-no-smtp-server-warning* nil)
(defvar *allow-passwordless-login* nil)

(defclass login-request ()
  ((email        :type string
                 :initarg :email
                 :initform (error "~s required for ~s" :email 'login-request)
                 :reader login-request/email
                 :persisted-p t)
   (created-date :type local-time:timestamp
                 :initform (local-time:now)
                 :reader login-request/created-date
                 :persisted-p t
                 :persisted-type :date)
   (key          :type string
                 :initform (potato.core::make-random-name 20)
                 :reader login-request/key
                 :persisted-p t)
   (user         :type (or null string)
                 :initarg :user
                 :initform (error "~s required for ~s" :user 'login-request)
                 :reader login-request/user
                 :persisted-p t)
   (save         :type (or null (eql t))
                 :initarg :save-login
                 :initform nil
                 :reader login-request/save-login
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

(defun register2-post-handler (email)
  (let ((email (string-downcase (potato.core:trim-string email))))
    (cond ((zerop (length email))
           (hunchentoot:redirect "/"))
          ((not (potato.core:is-allowed-email-p email))
           (error "Illegal email address format for ~s" email))
          (t
           (let ((id (make-login-request-id email)))
             (clouchdb:delete-document id :if-missing :ignore)
             (let ((user (potato.core:load-user-by-email email :error-if-not-found nil)))
               (when (and user (string/= (user/password user) ""))
                 (error "User has a password"))
               (let ((req (make-instance 'login-request :couchdb-id id :email email :user (if user (user/id user) nil) :save-login t)))
                 (potato.db:save-instance req)
                 (let ((url (potato.core:make-potato-url "register2_accept?email=~a&code=~a"
                                                         (encode-name email) (login-request/key req))))
                   (if *smtp-server-host*
                       (send-login-email email url (login-request/key req))
                       (log:warn "No SMTP server specified, login URL: ~a" url))
                   (lofn:show-template-stream "register2_loginkey.tmpl" `((:email . ,email)))))))))))

(lofn:define-handler-fn (register2-screen "/register2" nil ())
  (unless *allow-passwordless-login*
    (raise-not-found-error "Illegal registration mode"))
  (lofn:case-method
    (:get (lofn:show-template-stream "register2.tmpl" nil))
    (:post (lofn:with-parameters (email) (register2-post-handler email)))))

(defun verify-login-request (email code)
  (let ((req (potato.db:load-instance 'login-request (make-login-request-id email) :error-if-not-found nil)))
    (when req
      (clouchdb:delete-document (potato.db:persisted-entry/couchdb-id req))
      (when (string= (login-request/key req) code)
        req))))

(defun make-registered-and-activated-user (email &key description)
  (potato.workflow:register-user email (or description (make-description-from-email email)) nil nil t))

(defun login-and-maybe-create-new-user (req)
  (alexandria:if-let ((user-id (login-request/user req)))
    (let ((user (load-user user-id)))
      (potato.core:setup-user-session user (login-request/save-login req)))
    ;; ELSE: Create the user
    (let ((user (make-registered-and-activated-user (login-request/email req))))
      (potato.core:setup-user-session user (login-request/save-login req)))))

(lofn:define-handler-fn (register2-accept-screen "/register2_accept" nil ())
  (lofn:with-parameters (email code)
    (alexandria:if-let ((req (verify-login-request email code)))
      (progn
        (log:trace "Processing login request for: ~s" req)
        (login-and-maybe-create-new-user req)
        (hunchentoot:redirect "/"))
      ;; ELSE: Send back to login screen
      (hunchentoot:redirect "/register2"))))
