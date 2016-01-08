(in-package :potato.core)

(declaim #.potato.common::*compile-decl*)

(deftype byte-vector () '(vector (unsigned-byte 8) *))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conditions. Perhaps these definitions belong in its own file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition potato-error (error)
  ((message :type string
            :initarg :message
            :initform "Unknown message"
            :reader potato-error/message))
  (:report (lambda (condition stream)
             (format stream "Potato error: ~a" (potato-error/message condition))))
  (:documentation "Common superclass for errors thrown by web functions"))

(defgeneric potato-error/response-status (condition)
  (:method ((condition t)) hunchentoot:+http-internal-server-error+)
  (:documentation "The response status that this error should return to the HTTP client"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition permission-error (potato-error)
  ()
  (:documentation "Error that is raised when permissions are violated"))

(defmethod potato-error/response-status ((condition permission-error))
  hunchentoot:+http-forbidden+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition not-logged-in-error (potato-error)
  ()
  (:documentation "Error that is raised when an action which requres login in performed."))

(defmethod potato-error/response-status ((condition not-logged-in-error))
  hunchentoot:+http-authorization-required+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition custom-response-status-mixin (condition)
  ((status :type integer
           :initarg :status
           :reader custom-response-status-mixin/status
           :initform hunchentoot:+http-internal-server-error+)))

(defmethod potato-error/response-status ((condition custom-response-status-mixin))
  (custom-response-status-mixin/status condition))

(define-condition web-parameter-error (potato-error custom-response-status-mixin)
  ()
  (:documentation "Error that is raised when the input parameters to a web function are incorrect"))

(define-condition uri-not-found-error (potato-error custom-response-status-mixin)
  ()
  (:documentation "Error that is raised when http error should be set to not found"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun raise-permission-error (message &rest args)
  (error 'permission-error :message (apply #'format nil message args)))

(defun raise-web-parameter-error (message &rest args)
  (error 'web-parameter-error :status hunchentoot:+http-not-acceptable+ :message (apply #'format nil message args)))

(defun raise-not-found-error (message &rest args)
  (error 'uri-not-found-error :status hunchentoot:+http-not-found+ :message (apply #'format nil message args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Various random functions which have no better home
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro cd-row->list (row &rest fields)
  (let ((row-sym (gensym "ROW-")))
     `(let ((,row-sym ,row))
       (list ,@(loop
                  for field in fields
                  unless (stringp field)
                  do (error "Fields need to be strings")
                  collect `(cons ,(intern (string-upcase (substitute #\- #\_ field)) "KEYWORD")
                                 (getfield ,(intern field "KEYWORD") ,row-sym)))))))

(defun trim-string (string)
  (string-trim +BLANK-CHARS+ string))

(defun parse-timestamp (s)
  (local-time:parse-timestring s))

(defun format-timestamp (stream ts)
  (local-time:format-timestring stream ts
                                :format (append local-time:+iso-8601-date-format+
                                                          '("T")
                                                          local-time:+iso-8601-time-format+
                                                          '("Z"))
                                :timezone local-time:+utc-zone+))

(defvar *inhibit-lparallel* nil
  "If true, run futures in the same thread. Used for testcases.")

(defmacro run-future (&body body)
  (alexandria:with-gensyms (body-fn)
    `(flet ((,body-fn () ,@body))
       (if *inhibit-lparallel*
           (,body-fn)
           (lparallel:future
             (,body-fn))))))

(defun call-clouchdb-invoke-view* (id view &rest options &key key keys start-key
                                                           start-key-docid end-key end-key-docid limit stale
                                                           descending skip group group-level reduce
                                                           include-docs)
  "Invoke a view by specifiying the document ID that contains the view
and the name of the contained view. The key parameter specifies an
optional value to match against the view's mapped field. The start-key
and end-key values specify the optional begin and end range of the
mapped field(s) of each document to return. If descending is t,
returns results in reverse order. If update is t, does not refresh
view for query, use for higher performance but possible data
inconsistency."
  (declare (ignore key keys start-key start-key-docid end-key end-key-docid
                   limit stale descending skip group group-level
                   reduce include-docs))
  (clouchdb::ensure-db
    (clouchdb::db-request (clouchdb::cat (clouchdb::url-encode (clouchdb::db-name clouchdb:*couchdb*)) "/_design/" 
                                         (clouchdb::url-encode id) "/_view/" (clouchdb::url-encode view))
                          :method :get
                          :parameters (clouchdb::transform-params options clouchdb::*view-options*))))

(pushnew '(:keys . ((:name . "keys") (:fn . clouchdb:document-to-json)))
         clouchdb::*view-options* :key #'car)

(defun copy-alist-with-replacement (src key replacement &key (test #'eql))
  (loop
     for row in src
     for tag = (car row)
     when (funcall test tag key)
     collect (cons tag replacement)
     else collect row))

(defun is-allowed-email-p (email)
  (cl-ppcre:scan "^[^@]+@[^@]+$" email))

(defun make-potato-url (suffix-format &rest suffix-args)
  (with-output-to-string (s)
    (if *external-listen-address*
        (progn
          (princ *external-listen-address* s)
          (unless (eql (aref *external-listen-address* (1- (length *external-listen-address*))) #\/)
            (princ "/" s)))
        ;; ELSE: No external listen address set, use default
        (princ "http://localhost:8080/" s))
    (apply #'format s suffix-format suffix-args)))
