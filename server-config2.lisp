(defpackage #:potato-override
  (:use :cl))
(in-package #:potato-override)
(defparameter *override-decls* '(optimize (speed 3) (safety 1)))
;;(defparameter *override-decls* '(optimize (speed 0) (safety 3) (debug 3)))

(defpackage #:server-config2
  (:use :cl)
  (:export #:launch-service
           #:*default-upload-location*))
(in-package #:server-config2)

(defparameter *default-log-level* nil)
(defparameter *default-modules* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (truename "."))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init)))

  (let ((package (find-package "ASDF")))
    (when package
      (let ((sym (find-symbol "*CENTRAL-REGISTRY*" package)))
        (when sym
          (setf (symbol-value sym)
                (append '((merge-pathnames #p"vendor/cl-solr/" (truename "."))
                          (merge-pathnames #p"vendor/containers/" (truename "."))
                          (merge-pathnames #p"vendor/html5-notification/" (truename "."))
                          (merge-pathnames #p"vendor/lofn/" (truename "."))
                          (merge-pathnames #p"vendor/cl-markup/" (truename "."))
                          (merge-pathnames #p"vendor/clouchdb-patch/src/" (truename "."))
                          (merge-pathnames #p"vendor/cl-rabbit/" (truename "."))
                          (merge-pathnames #p"vendor/cl-rabbit-async/" (truename "."))
                          (merge-pathnames #p"vendor/zs3/" (truename "."))
                          (merge-pathnames #p"vendor/mailgun/" (truename "."))
                          (truename "."))
                        (symbol-value sym)))))))

  (ql:quickload "potato")
  (ql:quickload "swank")
  (ql:quickload "unix-opts"))

(setq potato.common:*debug* nil)
(setq potato.common:*potato-standalone* t)

(defun potato-write-pid (name)
  (with-open-file (s (merge-pathnames (concatenate 'string name ".pid") (user-homedir-pathname))
                     :direction :output :if-does-not-exist :create :if-exists :supersede)
    (format s "~a~%" (sb-posix:getpid))))

(defun parse-log-level (arg)
  (string-case:string-case (arg)
    ("TRACE" :trace)
    ("DEBUG" :debug)
    ("INFO" :info)
    ("WARNING" :warn)
    ("ERROR" :error)))

(defun parse-port-number (arg)
  (let ((number (parse-integer arg)))
    (unless (< 0 number 32768)
      (error "Invalid port number"))
    number))

(opts:define-opts
    (:name :potato
     :description "Start potato web server"
     :long "potato")
    (:name :state-server
     :description "Start state server"
     :long "state-server")
  (:name :index
   :description "Start the index manager"
   :long "index")
  (:name :content-processor
   :description "Start the content processor"
   :long "content-processor")
  (:name :email-updates
   :description "Start the email updates server"
   :long "email-updates")
  (:name :full
   :description "Start an instance with all services running in the same process"
   :long "full")
  (:name :debug
   :description "Run the instance in debug mode. Used for development of Potato"
   :long "debug")
  (:name :allowed-origin
   :description "When debug mode is on, this flag controls the value of the allowed-origin header"
   :long "allowed-origin"
   :arg-parser #'identity)
  (:name :cmd
   :description "Run administrative command"
   :long "cmd"
   :arg-parser #'identity)
  (:name :init
   :description "Initialise the database and exit the application"
   :long "init")
  (:name :config
   :description "Name of the configuration file (default: potato.cfg)"
   :short #\c
   :long "config"
   :arg-parser #'identity)
  (:name :swank-port
   :short #\p
   :description "Port number swank should be listening on"
   :long "swank-port"
   :arg-parser #'parse-port-number)
  (:name :service-name
   :description "Name of the service (used to form the pid file name and default log file name)"
   :short #\s
   :long "service-name"
   :arg-parser #'identity)
  (:name :log-location
   :description "Name of the log file"
   :short #\l
   :long "log-location"
   :arg-parser #'identity)
  (:name :log-level
   :description "Log level (TRACE, DEBUG, INFO, WARNING, ERROR)"
   :long "log-level"
   :arg-parser #'parse-log-level)
  (:name :port
   :description "Port number for the HTTP listener (used by the web server)"
   :short #\P
   :long "http-port"
   :arg-parser #'parse-port-number)
  (:name :websocket-port
   :description "Port number for the websocket server (used by the web server)"
   :short #\e
   :long "websocket-port"
   :arg-parser #'parse-port-number)
  (:name :help
   :description "Print help text"
   :short #\h
   :long "help"))

(defmacro with-readable-errors (&body body)
  (let ((condition (gensym)))
   `(handler-case
        (progn ,@body)
      (error (,condition)
        (format *error-output* "Error: ~a~%" ,condition)
        (uiop:quit 1)))))

(defun display-usage ()
  (opts:describe :prefix "Launch a potato server"))

(defun valid-hostname-p (name)
  (stringp name))

(defun hostname-or-nil-p (name)
  (or (null name) (valid-hostname-p name)))

(defun valid-port-p (port)
  (and (integerp port)
       (< 0 port 32768)))

(defun valid-url-p (url)
  (and (stringp url) (plusp (length url))))

(defun string-or-nil-p (v)
  (or (null v) (stringp v)))

(defun convert-path (name)
  (uiop:ensure-pathname name :ensure-directory t))

(defun valid-upload-location-p (value)
  (member value '(:s3 :file)))

(defun load-config (file)
  (with-open-file (s file :if-does-not-exist nil)
    (if s
        (let ((result (read s)))
          (macrolet ((update (sym var validator &optional converter)
                       (let ((res (gensym))
                             (content (gensym)))
                         `(alexandria:when-let ((,res (assoc ,sym result)))
                            (let ((,content (cdr ,res)))
                              ,(if validator
                                   `(if (funcall ,validator ,content)
                                        (setq ,var ,(if converter `(funcall ,converter ,content) content))
                                        (log:warn "Illegal value for config key ~s: ~s" ,sym ,content))
                                   `(setq ,var ,(if converter `(funcall ,converter ,content) content))))))))
            (update :db-host potato.common:*db-hostname* #'valid-hostname-p)
            (update :db-port potato.common:*db-port* #'valid-port-p)
            (update :db-name potato.common:*db-name* #'stringp)
            (update :db-user potato.common:*db-user* #'string-or-nil-p)
            (update :db-password potato.common:*db-password* #'string-or-nil-p)
            (update :email-type potato:*email-type* (lambda (v) (member v '(nil :smtp :mailgun))))
            (update :smtp-host potato:*smtp-server-host* #'hostname-or-nil-p)
            (update :smtp-port potato:*smtp-server-port* #'valid-port-p)
            (update :smtp-username potato:*smtp-username* #'string-or-nil-p)
            (update :smtp-password potato:*smtp-password* #'string-or-nil-p)
            (update :smtp-ssl potato:*smtp-ssl* (lambda (v) (member v '(nil :tls :starttls))))
            (update :mailgun-key potato:*mailgun-key* #'string-or-nil-p)
            (update :mailgun-user-domain potato:*mailgun-user-domain* #'string-or-nil-p)
            (update :mail-sender potato.email:*potato-sender-address* #'stringp)
            (update :external-listen-address potato:*external-listen-address* #'valid-url-p)
            (update :external-websocket-listen-address potato:*external-websocket-listen-address* #'valid-url-p)
            (update :solr-path potato.common:*solr-path-location* #'stringp)
            (update :memcached-host potato.common.memcached:*memcached-hostname* #'valid-hostname-p)
            (update :memcached-port potato.common.memcached:*memcached-port* #'valid-port-p)
            (update :listen-port potato:*listen-port* #'valid-port-p)
            (update :websocket-listen-port potato:*websocket-listen-port* #'valid-port-p)
            (update :rabbitmq-host potato.common:*rabbitmq-server-name* #'valid-hostname-p)
            (update :rabbitmq-port potato.common:*rabbitmq-server-port* #'valid-port-p)
            (update :rabbitmq-user potato.common:*rabbitmq-user* #'stringp)
            (update :rabbitmq-password potato.common:*rabbitmq-password* #'stringp)
            (update :rabbitmq-vhost potato.common:*rabbitmq-vhost* #'stringp)
            (update :s3-bucket potato.common:*s3-bucket* #'stringp)
            (update :s3-directory potato.common:*s3-directory* #'stringp)
            (update :s3-secret-key potato.common:*s3-secret-key* #'string-or-nil-p)
            (update :s3-access-key potato.common:*s3-access-key* #'string-or-nil-p)
            (update :s3-browser-secret-key potato.common:*s3-browser-secret-key* #'string-or-nil-p)
            (update :s3-browser-access-key potato.common:*s3-browser-access-key* #'string-or-nil-p)
            (update :s3-endpoint potato.common:*s3-endpoint* #'string-or-nil-p)
            (update :logging *default-log-level* (lambda (v) (member v '(:error :warn :info :debug :trace))))
            (update :imagemagick-convert-program potato.image-convert:*imagemagick-convert-program* #'stringp)
            (update :max-message-size potato.core:*max-message-size* #'alexandria:positive-integer-p)
            (update :modules *default-modules* nil)
            (update :youtube-key potato.content-processor.youtube:*youtube-key* #'string-or-nil-p)
            (update :allow-create-domain potato.web:*allow-create-domain* (constantly t))
            (update :upload-path potato.upload:*file-upload-directory* #'string-or-nil-p #'convert-path)
            (update :default-upload-mode potato.upload:*default-upload-location* #'valid-upload-location-p)
            (update :gcm-key potato.gcm:*gcm-authorisation-key* #'string-or-nil-p)
            (update :gcm-sender potato.gcm:*gcm-sender* #'string-or-nil-p)
            (update :allow-passwordless-login potato:*allow-passwordless-login* (constantly t))
            (update :allow-password-reset potato:*allow-password-recovery* (constantly t))
            (update :force-https potato.common:*force-https* (constantly t))
            (update :allow-user-no-domain potato:*allow-registration-without-domain* (constantly t)))
          result)
        ;; ELSE: Config file not found, print a warning
        (progn
          (log:warn "Configuration file \"~a\" not found" file)
          nil))))

(defun launch-service ()
  (multiple-value-bind (options free-args)
      (handler-case
          (opts:get-opts)
        (opts:missing-arg (condition)
          (format *error-output* "Error: option ~s needs an argument~%~%"
                  (opts:option condition))
          (display-usage)
          (uiop:quit 1))
        (opts:arg-parser-failed (condition)
          (format *error-output* "Error: can't parse ~s as argument of ~s~%~%"
                  (opts:raw-arg condition) (opts:option condition))
          (display-usage)
          (uiop:quit 1)))

    (when (or free-args
              (getf options :help))
      (display-usage)
      (uiop:quit 0))

    (load-config (or (getf options :config) "potato.cfg"))

    (let ((service-name (getf options :service-name)))
      (when service-name
        (setq potato.common:*log-location*
              (or (getf options :log-location)
                  (concatenate 'string service-name ".log"))))

      (alexandria:when-let ((value (getf options :log-level)))
        (setq *default-log-level* value))
      (when *default-log-level*
        (log4cl:set-log-level log4cl:*root-logger* *default-log-level*))

      (alexandria:when-let ((value (getf options :port)))
        (setq potato:*listen-port* value))

      (alexandria:when-let ((value (getf options :websocket-port)))
        (setq potato:*websocket-listen-port* value))

      (when (getf options :debug)
        (setq potato.common:*debug* t))

      (alexandria:when-let ((value (getf options :allowed-origin)))
        (setq potato:*allowed-origin* value))

      (dolist (module *default-modules*)
        (destructuring-bind (init-fn &key parameters) module
          (unless (symbolp init-fn)
            (format *error-output* ":MODULE does not specify a symbol: ~s" module))
          (funcall init-fn parameters)))

      (alexandria:when-let ((cmd (getf options :cmd)))
        (potato.common.application:start-component 'potato.common::generic)
        (with-readable-errors
          (potato.commands:run-command cmd))
        (uiop:quit 0))

      (labels ((init-and-start (service &rest more-services)
                 (potato.common.application:start-component service)
                 (dolist (s more-services)
                   (potato.common.application:start-component s))))
        (cond ((getf options :potato)
               (init-and-start 'potato::main))
              ((getf options :state-server)
               (init-and-start 'state-server::state-server))
              ((getf options :index)
               (init-and-start 'potato-index::index-manager))
              ((getf options :content-processor)
               (init-and-start 'potato::message-processor-server))
              ((getf options :email-updates)
               (init-and-start 'potato::email-updates-server 'potato.gcm::gcm-sender))
              ((getf options :full)
               (init-and-start 'potato::all-services))
              ((getf options :init)
               (potato:setup-initial-database)
               (format t "Database was initialised successfully~%")
               (uiop:quit 0))
              (t
               (format *error-output* "Error: No service specified~%~%")
               (display-usage)
               (uiop:quit 1))))

      (alexandria:when-let ((value (getf options :swank-port)))
        (swank:create-server :port value :dont-close t))
      (when service-name
        (potato-write-pid service-name))
      (loop do (sleep 10000)))))
