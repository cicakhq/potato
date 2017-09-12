(in-package :potato.core)

(declaim #.potato.common::*compile-decl*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq lofn:*files-base-dir* (merge-pathnames #p"src/" (asdf:component-pathname (asdf:find-system :potato)))))

(defvar *session-cookie-name* "potato-session")
(defvar *current-user-session* nil)

(defclass potato-acceptor (lofn:server-acceptor)
  ()
  (:documentation "Acceptor for the potato server"))

(defmethod initialize-instance :after ((obj potato-acceptor) &key)
  (setf (hunchentoot:acceptor-request-class obj) 'potato-web-request))

(defmethod hunchentoot:process-connection ((acceptor potato-acceptor) (socket t))
  (handler-bind ((error (lambda (condition)
                          (log:warn "Error of type: ~s" (type-of condition)))))
    (call-next-method)))

(defmethod hunchentoot:acceptor-log-message ((acceptor potato-acceptor) log-level fmt &rest args)
  (log:trace "backtrace:~a" (with-output-to-string (s) (trivial-backtrace:print-backtrace-to-stream s)))
  (let ((message (apply #'format nil fmt args)))
    (case log-level
      (:error   (log:error "~a" message))
      (:warning (log:info "~a" message))
      (:info    (log:info "~a" message))
      (t        (log:error (format nil "Unknown hunchentoot log level type. Msg=~a" message))))))

(defmethod hunchentoot:handle-request ((acceptor potato-acceptor) request)
  (with-memcached-warnings-muffled
    (let ((*current-user-session* nil))
      ;; In debug mode, we allow all CORS requests
      (when (and *debug* *allowed-origin*)
        (setf (hunchentoot:header-out :access-control-allow-origin) *allowed-origin*)
        (setf (hunchentoot:header-out :access-control-allow-credentials) "true"))
      (when *force-https*
        (setf (hunchentoot:header-out :strict-transport-security) "max-age=15768000"))
      (setf (hunchentoot:header-out :referrer-policy) "no-referrer")
      (call-next-method))))

(defclass potato-web-request (hunchentoot:request)
  ()
  (:documentation "Request object for potato hunchentoot requests"))

(defclass persisted-user-session ()
  ((user                 :type (or null string)
                         :initarg :user-id
                         :accessor persisted-user-session/user-id)
   (activation-confirmed :type t
                         :initarg :activation-confirmed
                         :accessor persisted-user-session/activation-confirmed)
   (session-data         :type list
                         :initarg :session-data
                         :accessor persisted-user-session/session-data)))

(defmethod print-object ((obj persisted-user-session) stream)
  (print-unreadable-safely (user activation-confirmed) obj stream
    (format stream "USER ~s ACTIVATION-CONFIRMED ~s" user activation-confirmed)))

(defclass user-session ()
  ((cached-user           :type (or null user (eql :unset))
                          :initform :unset
                          :accessor user-session/cached-user)
   (user-id               :type (or null string)
                          :initarg :user-id
                          :initform nil
                          :accessor user-session/user-id)
   (cookie-value          :type string
                          :initarg :cookie-value
                          :reader user-session/cookie-value)
   (activation-confirmed  :type t
                          :initarg :activation-confirmed
                          :initform nil
                          :accessor user-session/activation-confirmed)
   (session-data          :type list
                          :initarg :session-data
                          :initform nil
                          :accessor user-session/session-data
                          :documentation "Extra fields available to plugins"))
  (:documentation "Class holding the session information"))

(defmethod print-object ((obj user-session) stream)
  (print-unreadable-safely (user-id cached-user cookie-value) obj stream
    (format stream "USER ~s COOKIE ~s HAS-CACHED ~a" user-id cookie-value (case cached-user
                                                                            (nil "nil")
                                                                            (:unset "unset")
                                                                            (t "set")))))

(declaim (inline session-value))
(defun session-value (session key)
  (getfield key (user-session/session-data session) :accept-missing t))

(declaim (inline (setf session-value)))
(defun (setf session-value) (value session key)
  (if value
      (alexandria:if-let ((v (assoc key (user-session/session-data session))))
        (setf (cdr v) value)
        (push (cons key value) (user-session/session-data session)))
      (setf (user-session/session-data session)
            (delete key (user-session/session-data session)))))

(defgeneric user-session/user (session))

(defmethod user-session/user ((session user-session))
  (with-slots (cached-user user-id) session
    (if (eq cached-user :unset)
        (let ((user (if user-id (load-user user-id) nil)))
          (setf cached-user user)
          user)
        cached-user)))

(defun user-session/activated-p (session)
  (or (user-session/activation-confirmed session)
      (let* ((user (user-session/user session))
             (activated-p (user/activated-p user)))
        (if activated-p
            (progn
              (setf (user-session/activation-confirmed session) activated-p)
              (update-persisted-session-data session)
              activated-p)
            nil))))

(defun make-memcached-session-cookie (name)
  (concatenate 'string "login-session-" name))

(defun %load-persisted-user-session (cookie)
  (let* ((key (make-memcached-session-cookie cookie))
         (result (cl-memcached:mc-get (list key))))
    (when result
      (let* ((value (fifth (car result)))
             (persisted (potato.common:decode-conspack-with-interning value)))
        (cl-memcached:mc-set key value :timeout (* 30 60))
        persisted))))

(defun load-persisted-user-session (cookie)
  (let ((session (%load-persisted-user-session cookie)))
    (log:trace "Loaded persisted session for cookie: ~s. result=~s" cookie session)
    session))

(defun update-persisted-session-data (session)
  (log:trace "Updating session data in cache: ~s" session)
  (let ((persisted (make-instance 'persisted-user-session
                                  :user-id (user-session/user-id session)
                                  :activation-confirmed (user-session/activation-confirmed session)
                                  :session-data (user-session/session-data session)))
        (cookie-value (user-session/cookie-value session)))
    (cl-memcached:mc-set (make-memcached-session-cookie cookie-value)
                         (conspack:encode persisted) :timeout (* 30 60))))

(defun %find-current-user-session ()
  (flet ((create-new-session ()
           (let ((session (make-instance 'user-session
                                         :user-id nil
                                         :cookie-value (ironclad:byte-array-to-hex-string (secure-random:bytes 64 secure-random:*generator*)))))
             (update-persisted-session-data session)
             (hunchentoot:set-cookie *session-cookie-name* :value (user-session/cookie-value session)
                                                           :path "/"
                                                           :secure *force-https*
                                                           :http-only t)
             session)))

    (or *current-user-session*
        (let ((cookie (hunchentoot:cookie-in *session-cookie-name*)))
          (setq *current-user-session*
                (if cookie
                    (let ((persisted (load-persisted-user-session cookie)))
                      (if persisted
                          (make-instance 'user-session
                                         :user-id (persisted-user-session/user-id persisted)
                                         :cookie-value cookie
                                         :activation-confirmed (persisted-user-session/activation-confirmed persisted)
                                         :session-data (persisted-user-session/session-data persisted))
                          (create-new-session)))
                    ;; No cookie value, create a new session
                    (create-new-session)))))))

(defun find-current-user-session ()
  (let ((result (%find-current-user-session)))
    (log:trace "Finding user session, result: ~s" result)
    result))

(defun update-user-session (user)
  (check-type user (or null user))
  (let ((session (find-current-user-session)))
    (with-slots (cached-user user-id activation-confirmed) session
      (setf user-id (if user (user/id user) nil))
      (setf cached-user user)
      (setf activation-confirmed (if (and user (user/activated-p user)) t nil))
      (update-persisted-session-data session))))
