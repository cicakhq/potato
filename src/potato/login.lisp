(in-package :potato.core)

(declaim #.potato.common::*compile-decl*)

(defvar *current-auth-user* nil
  "Dynamic variable holding a reference to the current user")

(defvar *authenticator-function* nil)

(defvar *external-user-check-function* nil
  "If set, this variable should point to a function that is
responsible for handling authentication for a user. This function will
be called with two parameters: the username and the password. A true
result from this function indicates that the username and passwords
are correct.")

(defparameter *login-reference-prefix* "loginkey")

(defclass login-reference ()
  ((user    :type string
            :initarg :user
            :reader login-reference/user
            :persisted-p t)
   (key     :type string
            :initarg :key
            :reader login-reference/key
            :persisted-p t)
   (expires :type integer
            :initarg :expires
            :initform (+ (get-universal-time) (* 30 24 60 60))
            :reader login-reference/expires
            :persisted-p t
            :persisted-type :integer))
  (:metaclass potato.db:persisted-entry-class))

(defmethod initialize-instance :after ((obj login-reference) &key &allow-other-keys)
  (setf (potato.db:persisted-entry/couchdb-id obj) (make-login-reference-id (login-reference/user obj)
                                                                            (login-reference/key obj))))

(defun generate-random-login-reference ()
  (ironclad:byte-array-to-hex-string (secure-random:bytes 64 secure-random:*generator*)))

(defun show-login-screen (username error-p redirect)
  (lofn:with-parameters ((mobile "m"))
    (lofn:show-template-stream "login.tmpl" (list (cons :username username)
                                                  (cons :error-p error-p)
                                                  (cons :mobile-p (equal mobile "1"))
                                                  (cons :redirect redirect)
                                                  (cons :use-external-authenticator (not (null *authenticator-function*)))))))

(defun make-login-reference-id (user key)
  (format nil "~a-~a:~a" *login-reference-prefix* (encode-name user) (encode-name key)))

(defun setup-user-session (user remember)
  (update-user-session user)
  (when remember
    (let ((ref (make-instance 'login-reference
                              :user (user/id user)
                              :key (generate-random-login-reference))))
      (potato.db:save-instance ref)
      (hunchentoot:set-cookie "loginkey"
                              :value (format nil "~a:~a"
                                             (encode-name (login-reference/user ref))
                                             (encode-name (login-reference/key ref)))
                              :expires (login-reference/expires ref)
                              :path "/"
                              :secure *force-https*
                              :http-only t))))

(defun load-ref-from-cookie-value (value)
  (multiple-value-bind (match strings)
      (cl-ppcre:scan-to-strings "^([^:]+):([^:]+)$" value)
    (when match
      (let ((ref (potato.db:load-instance 'login-reference (make-login-reference-id (decode-name (aref strings 0))
                                                                                    (decode-name (aref strings 1)))
                                          :error-if-not-found nil)))
        ref))))

(defun load-ref-from-cookie ()
  (load-ref-from-cookie-value (hunchentoot:cookie-in "loginkey")))

(defun load-autologin-user (cookie)
  (let ((ref (load-ref-from-cookie-value cookie)))
    (when (and ref (< (get-universal-time) (login-reference/expires ref)))
      (let ((user (potato.db:load-instance 'user (login-reference/user ref) :error-if-not-found nil)))
        (when user
          (update-user-session user)
          user)))))

(defun validate-cookie ()
  "Check the session cookie to determine the user that has been logged
in. If there is no current user, this function also checks the
autologin cookie and if set, will log in the user using those
credentials. This function returns the current session."
  (let ((session (find-current-user-session)))
    (unless (user-session/user-id session)
      ;; If the user is not logged in, check the autologin cookie value
      (let ((cookie (hunchentoot:cookie-in "loginkey")))
        (when (and cookie (not (equal cookie "")))
          (unless (load-autologin-user cookie)
            ;; We had a cookie, but there was no login data or the data has expired
            (hunchentoot:set-cookie "loginkey" :value "" :path "/" :secure *force-https* :http-only t)))))
    session))

(defun validate-cookie-and-find-user ()
  (validate-cookie)
  (let* ((user (user-session/user (find-current-user-session))))
    (if (and user (user/activated-p user))
        user
        nil)))

(defmacro with-authenticated-user ((&optional allow-unregistered) &body body)
  (alexandria:with-gensyms (user-session-sym user-sym)
    ;; We bind *CURRENT-USER-SESSION* to itself in order to ensure a
    ;; local binding for this variable. However, it would be nicer if
    ;; FIND-CURRENT-USER-SESSION could ensure this by itself.
    `(let* ((*current-user-session* *current-user-session*)
            (,user-session-sym (find-current-user-session))
            (,user-sym (user-session/user ,user-session-sym)))
       (cond (,(if allow-unregistered user-sym `(and ,user-sym (user/activated-p ,user-sym)))
              (let ((*current-auth-user* ,user-sym))
                ,@body))
             (,user-sym
              (hunchentoot:redirect "/user_not_activated"))
             (t
              (log:trace "Throwing not-logged-in-error, session = ~s" ,user-session-sym)
              (error 'not-logged-in-error))))))

(defun current-user ()
  (unless *current-auth-user*
    (log:info (with-output-to-string (s) (trivial-backtrace:print-backtrace-to-stream s)))
    (error "Attempt to access logged in user outside of authentication form"))
  *current-auth-user*)

(defun create-user-if-needed-and-update-session (user-fields)
  (log:trace "User authenticated. username=~s, description=~s" (first user-fields) (second user-fields))
  (let ((user (load-user-by-email (car user-fields) :error-if-not-found nil)))
    (unless user
      (setq user (potato.workflow:register-user (first user-fields) (second user-fields) nil nil t)))
    (setup-user-session user nil)
    user))

(defun call-with-login-handler (body-fn)
  (labels ((authenticate-success (user-fields)
             (create-user-if-needed-and-update-session user-fields)
             (funcall body-fn))

           (authenticate-failed ()
             (let* ((uri (hunchentoot:request-uri*))
                    (request (base64:usb8-array-to-base64-string (babel:string-to-octets uri :encoding :utf-8))))
               (show-login-screen nil nil request))))

    (let ((session (validate-cookie)))
      (if (and *authenticator-function* (not (user-session/user-id session)))
          (funcall *authenticator-function* #'authenticate-success #'authenticate-failed)
          ;; ELSE: User logged in or no authenticator function
          (handler-case
              (funcall body-fn)
            (not-logged-in-error ()
              (authenticate-failed)))))))

(defmacro define-handler-fn-login ((name url regex (&rest bind-vars)) &body body)
  `(lofn:define-handler-fn (,name ,url ,regex ,bind-vars)
     (call-with-login-handler (lambda () ,@body))))

(defmacro define-json-handler-fn-login ((name url data-symbol regex (&rest bind-vars)) &body body)
  (multiple-value-bind (rem-forms declarations docstring)
      (alexandria:parse-body body :documentation t)
    (let ((condition-sym (gensym "CONDITION-"))
          (stream-sym (gensym "STREAM-")))
      `(lofn:define-json-handler-fn (,name ,url ,data-symbol ,regex ,bind-vars)
         ,@(if docstring (list docstring))
         ,@declarations
         (validate-cookie)
         (block call-handlers
           (handler-bind ((not-logged-in-error (lambda (,condition-sym)
                                                 (declare (ignore ,condition-sym))
                                                 (setf (hunchentoot:return-code*)
                                                       hunchentoot:+http-authorization-required+)
                                                 (return-from call-handlers
                                                   (st-json:jso "result" "error"
                                                                "message" "not_logged_in"))))
                          (potato-error (lambda (,condition-sym)
                                          (setf (hunchentoot:return-code*)
                                                (potato-error/response-status ,condition-sym))
                                          (return-from call-handlers
                                            (st-json:jso "result" "error"
                                                         "message" "generic-error"
                                                         "description" (potato-error/message ,condition-sym)))))
                          (error (lambda (,condition-sym)
                                   (unless *debug*
                                     (log:error "Fatal error in json handler: ~a~%~a"
                                                ,condition-sym
                                                (with-output-to-string (,stream-sym)
                                                  (trivial-backtrace:print-backtrace-to-stream ,stream-sym)))
                                     (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
                                     (return-from call-handlers
                                       (st-json:jso "result" "error"
                                                    "message" "server-error"))))))
             (lofn:case-method
               (:post ,@rem-forms))))))))

(defun load-user-and-check-password-native (email password)
  (log:trace "Verifying user. email=~s password=~s" email password)
  (let ((user (load-user-by-email email :error-if-not-found nil)))
    (if (or (null user) (not (user/match-password user password)))
        (raise-permission-error "Username or password is incorrect")
        user)))

(defun load-user-and-check-password (email password)
  (if *external-user-check-function*
      (create-user-if-needed-and-update-session (funcall *external-user-check-function* email password))
      (load-user-and-check-password-native email password)))

(defun maybe-enable-api-and-redirect-to-mobile ()
  (with-authenticated-user ()
    (let ((user (current-user)))
      (when (empty-string-or-nil-p (user/api-token user))
        (generate-and-modify-api-token user)
        (save-user user))
      (hunchentoot:redirect (format nil "potato://authenticated?key=~a&user_id=~a"
                                    (hunchentoot:url-encode (user/api-token user))
                                    (hunchentoot:url-encode (user/id user)))))))

(defun handle-login ()
  (lofn:with-parameters (username password remember (mobile "m") redirect)
    (log:trace "Handling login for user: ~s" username)
    (handler-case
        (let ((user (load-user-and-check-password (string-downcase (trim-string username)) password)))
          (setup-user-session user remember)
          (if (equal mobile "1")
              (maybe-enable-api-and-redirect-to-mobile)
              (hunchentoot:redirect (if redirect
                                        (babel:octets-to-string (cl-base64:base64-string-to-usb8-array redirect)
                                                                :encoding :utf-8)
                                        "/"))))
      (permission-error ()
        (log:trace "Permission error when logging in user ~s using email/password" username)
        (show-login-screen username t redirect)))))

(lofn:define-handler-fn (login-screen "/login" nil ())
  (lofn:case-method
    (:get (show-login-screen nil nil (hunchentoot:parameter "redirect")))
    (:post (handle-login))))

(lofn:define-handler-fn (logout-screen "/logout" nil ())
  (update-user-session nil)
  (alexandria:when-let ((ref (load-ref-from-cookie)))
    (potato.db:remove-instance ref))
  (hunchentoot:set-cookie "loginkey" :value "" :path "/" :secure *force-https* :http-only t)
  (hunchentoot:redirect "/"))

(lofn:define-handler-fn (forgot-password-screen "/forgot-password" nil ())
  (lofn:case-method
    (:get (lofn:show-template-stream "forgot-password.tmpl" nil))
    (:post (lofn:with-checked-parameters ((email :name "email" :trimmed t :allow-blank nil :required t))
             (potato.register:register2-post-handler email t)))))
