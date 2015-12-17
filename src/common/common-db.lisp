(in-package :potato.common)

(declaim #.*compile-decl*)

(defvar *clouchdb-init-lock* (bordeaux-threads:make-lock "Couchdb initialisation lock"))

(defvar *db-hostname* "127.0.0.1"
  "The hostname of the couchdb server")

(defvar *db-port* 5984
  "The port number of the couchdb server")

(defvar *db-name* "foo"
  "The name of the couchdb database")

(defvar *db-user* nil
  "The name of the database user, or NIL if no user is specified.")

(defvar *db-password* nil
  "The database password, or NIL if not used.")

(defvar *main-db* nil
  "The main database. This is the standard value of CLOUCHDB:*COUCHDB*")

(defvar *user-notification-db* nil
  "The couchdb database that is used to store user notifications")

(defvar *messages-db* nil
  "The couchdb database that is used to store chat messages")

(potato.common.application:define-component clouchdb
  (:start
   (bordeaux-threads:with-lock-held (*clouchdb-init-lock*)
     (setq *main-db* (clouchdb:make-db :host *db-hostname*
                                       :port (princ-to-string *db-port*)
                                       :name *db-name*
                                       :user *db-user*
                                       :password *db-password*))
     (setq *user-notification-db* (clouchdb:make-db :host *db-hostname*
                                                    :port (princ-to-string *db-port*)
                                                    :name (concatenate 'string *db-name* "-notification")
                                                    :user *db-user*
                                                    :password *db-password*))
     (setq *messages-db* (clouchdb:make-db :host *db-hostname*
                                           :port (princ-to-string *db-port*)
                                           :name (concatenate 'string *db-name* "-messages")
                                           :user *db-user*
                                           :password *db-password*))
     (setq clouchdb:*couchdb* *main-db*)
     #+nil(setq clouchdb:*use-pool* nil))))

(defmacro with-main-db (&body body)
  "Evaluate BODY with the current clouchdb instance bound to the
standard database. This is only needed if the current database has
been changed from the default, for example by being run in the dynamic
scope of WITH-USER-NOTIFICATION-DB ot WITH-MESSAGES-DB."
  `(let ((clouchdb:*couchdb* *main-db*))
     ,@body))

(defmacro with-user-notification-db (&body body)
  "Evaluate BODY with the current clouchdb instance bound to the
notification database."
  `(let ((clouchdb:*couchdb* *user-notification-db*))
     ,@body))

(defmacro with-messages-db (&body body)
  "Evaluate BODY with the current clouchdb instance bound to the
messages database."
  `(let ((clouchdb:*couchdb* *messages-db*))
     ,@body))

(defun decode-conspack-with-interning (data)
  (conspack:with-interning ()
    (conspack:decode data)))
