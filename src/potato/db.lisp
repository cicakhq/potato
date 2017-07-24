(in-package :potato.db)

(declaim #.potato.common::*compile-decl*)

(define-condition persist-error (error)
  ((result :type list
           :initarg :result
           :reader persist-error/result))
  (:documentation "Error that is raised if a persist command fails"))

(define-condition couchdb-missing-value (error)
  ((couchdb-content :type list
                    :initarg :couchdb-content
                    :reader couchdb-missing-value/couchdb-content
                    :documentation "The document content")
   (name            :type symbol
                    :initarg :name
                    :reader couchdb-missing-value/name
                    :documentation "The value that was expected in the document"))
  (:report (lambda (condition stream)
             (format stream "Missing value in couchdb document. Expected field: ~s. Content: ~s"
                     (couchdb-missing-value/name condition)
                     (couchdb-missing-value/couchdb-content condition)))))

(defun init-supplementary-db ()
  (let* ((result (clouchdb:get-db-info))
         (message (cdr (assoc :|error| result))))
    (when (equal message "not_found")
      (clouchdb:create-db))))

(potato.common.application:define-component db
  (:dependencies potato.common::clouchdb)
  (:start
   (let* ((result (handler-case
                      (clouchdb:get-db-info)
                    (drakma:drakma-error ()
                      (error "Unable to connect to CouchDB"))))
          (message (cdr (assoc :|error| result))))
     (when (equal message "not_found")
       (clouchdb:create-db)))
   (with-user-notification-db
     (init-supplementary-db))
   (with-messages-db
     (init-supplementary-db))
   (potato.views:init-views-if-needed)))

(defgeneric value->persisted-value (type value)
  (:method ((type t) (value null))
    nil)
  (:method ((type list) (value t))
    (ecase (car type)
      (:list (if value
                 (mapcar #'(lambda (v)
                             (value->persisted-value (cadr type) v))
                         value)
                 'clouchdb:json-list))
      (:inst (make-doc-from-instance (class-of value) value))
      (:alist (let ((value-type (cadr type)))
                (loop
                  for (key . alist-val) in value
                  unless (keywordp key)
                    do (error "Keys in persisted alists must be keywords")
                  collect (cons (symbol-value key) (value->persisted-value value-type alist-val)))))))
  (:method ((type (eql :string)) (value string))
    value)
  (:method ((type (eql :string)) (value integer))
    (princ-to-string value))
  (:method ((type (eql :integer)) (value integer))
    value)
  (:method ((type (eql :integer)) (value real))
    (truncate value))
  (:method ((type (eql :symbol)) (value symbol))
    (check-type value keyword)
    (symbol-name value))
  (:method ((type (eql :json)) (value t))
    value)
  (:method ((type (eql :date)) (value local-time:timestamp))
    (format-timestamp nil value))
  (:method ((type (eql :boolean)) (value t))
    (if value t 'clouchdb:json-false)))

(defgeneric persisted-value->value (type value)
  (:method ((type t) (value null))
    nil)
  (:method ((type list) (value t))
    (ecase (car type)
      (:list (mapcar #'(lambda (v)
                         (persisted-value->value (cadr type) v))
                     value))
      (:inst (load-instance-from-doc (cadr type) value :accept-empty-type t))
      (:alist (let ((value-type (cadr type)))
                (loop
                  for (key . alist-val) in value
                  collect (cons key (persisted-value->value value-type alist-val)))))))
  (:method ((type (eql :string)) (value string))
    value)
  (:method ((type (eql :integer)) (value string))
    (parse-integer value))
  (:method ((type (eql :integer)) (value integer))
    value)
  (:method ((type (eql :integer)) (value real))
    (truncate value))
  (:method ((type (eql :symbol)) (value string))
    (intern value "KEYWORD"))
  (:method ((type (eql :json)) (value t))
    value)
  (:method ((type (eql :date)) (value string))
    (parse-timestamp value))
  (:method ((type (eql :boolean)) (value t))
    (cond
      ((null value) nil)
      ((eq value 'clouchdb:json-false) nil)
      ((eq value t) t)
      (t (error "Unexpected value for boolean: ~s" value)))))

(defun emit-clouchdb-list-for-slot (obj class slot)
  (when (persisted-entry-class/persisted-p slot)
    (list (cons (persisted-entry-class/field slot)
                (let ((v (closer-mop:slot-value-using-class class obj slot)))
                  (value->persisted-value (persisted-entry-class/persisted-slot-type slot) v))))))

(defun run-hooks-for-obj (obj type)
  (dolist (hook (persisted-entry-class/hooks (class-of obj)))
    (funcall hook obj type)))

(defun make-doc-from-instance (class obj)
  (loop
     for slot in (closer-mop:class-slots class)
     append (emit-clouchdb-list-for-slot obj class slot)))

(declaim (inline flush-cache-for-object-if-enabled))
(defun flush-cache-for-object-if-enabled (class id)
  (when (persisted-entry-class/memcached-enabled-p class)
    (cl-memcached:mc-del (make-instance-memcached-key (class-name class) id))))

(declaim (inline flush-cache-for-instance))
(defun flush-cache-for-instance (obj)
  (flush-cache-for-object-if-enabled (class-of obj) (persisted-entry/couchdb-id obj)))

(defgeneric save-instance (obj &key error-if-fail))

(defmethod save-instance ((obj persisted-entry) &key (error-if-fail t))
  (let* ((class (class-of obj))
         (args (make-doc-from-instance class obj)))
    (run-hooks-for-obj obj :pre-save)
    (let ((result (apply #'clouchdb:create-document
                         (append args
                                 (alexandria:if-let ((counchdb-revision (persisted-entry/couchdb-revision obj)))
                                   (list (cons :|_rev| counchdb-revision)))
                                 (alexandria:if-let ((couchdb-type (persisted-entry-class/couchdb-type class)))
                                   (list (cons :|type| couchdb-type)))
                                 (alexandria:if-let ((attachments (persisted-entry/couchdb-attachments obj)))
                                   (list (cons :|_attachments| attachments))))
                         (let ((id (persisted-entry/couchdb-id obj)))
                           (when id
                             (list :id id))))))
      (cond ((cdr (assoc :|ok| result))
             ;; The document was successfully created, update the id slot
             (setf (persisted-entry/couchdb-id obj) (cdr (assoc :|id| result)))
             (setf (persisted-entry/couchdb-revision obj) (cdr (assoc :|rev| result)))
             (flush-cache-for-object-if-enabled class (persisted-entry/couchdb-id obj))
             (run-hooks-for-obj obj :save)
             (potato.db:persisted-entry-clear-modifications-list obj))
            (error-if-fail
             ;; The create call failed, report the error unless ERROR-IF-FAIL is false
             (error 'persist-error :result result)))
      result)))

(defgeneric load-instance-from-doc (class doc &key accept-empty-type))

(defmethod load-instance-from-doc ((class symbol) doc &key accept-empty-type)
  (load-instance-from-doc (find-class class) doc :accept-empty-type accept-empty-type))

(defmethod load-instance-from-doc ((class class) doc &key accept-empty-type)
  (let ((obj (allocate-instance class)))
    (alexandria:when-let ((type (symbol-name (persisted-entry-class/couchdb-type class))))
      (when (and (not accept-empty-type)
                 (not (string= (getfield :|type| doc) type)))
        (error "Incorrect type. Expected: ~s. Got: ~s" type (getfield :|type| doc))))
    (unless accept-empty-type
      (setf (persisted-entry/couchdb-id obj) (getfield :|_id| doc))
      (setf (persisted-entry/couchdb-revision obj) (getfield :|_rev| doc)))
    (dolist (slot (closer-mop:class-slots class))
      (when (persisted-entry-class/persisted-p slot)
        (let ((value (assoc (persisted-entry-class/field slot) doc)))
          (when (and (not (persisted-entry-class/persisted-allow-missing-value slot))
                     (null value))
            (error 'couchdb-missing-value :name (persisted-entry-class/field slot) :couchdb-content doc))
          (setf (closer-mop:slot-value-using-class class obj slot)
                (if value
                    (persisted-value->value (persisted-entry-class/persisted-slot-type slot) (cdr value))
                    (persisted-entry-class/persisted-missing-default slot))))))
    (alexandria:when-let ((attachments (and (persisted-entry-class/attachments-p class)
                                            (assoc :|_attachments| doc))))
      (setf (slot-value obj 'couchdb-attachments) (cdr attachments)))
    (initialize-instance obj)
    obj))

(defun make-instance-memcached-key (class-id id)
  (format nil "db-~a:~a-~a" (package-name (symbol-package class-id)) (symbol-name class-id) id))

(defgeneric load-instance (class id &key error-if-not-found)
  (:method ((class symbol) id &key (error-if-not-found t))
    (load-instance (find-class class) id :error-if-not-found error-if-not-found))
  (:method ((class class) id &key (error-if-not-found t))
    (check-type id string)
    (let* ((memcached-enabled-p (persisted-entry-class/memcached-enabled-p class))
           (cached (and memcached-enabled-p (cl-memcached:mc-get (list (make-instance-memcached-key (class-name class) id))))))
      (if cached
          (let ((obj (potato.common:decode-conspack-with-interning (fifth (car cached)))))
            ;; Update the loaded flag since it is not persisted to memcached
            (setf (slot-value obj 'loaded-p) t)
            obj)
          ;; ELSE: Object not found in cache
          (let ((doc (clouchdb:get-document id :if-missing (if error-if-not-found :error :ignore))))
            (when doc
              (let ((obj (load-instance-from-doc (class-name class) doc)))
                (when memcached-enabled-p
                  (cl-memcached:mc-set (make-instance-memcached-key (class-name class) id) (conspack:encode obj)))
                (setf (slot-value obj 'loaded-p) t)
                obj)))))))

(defgeneric remove-instance (obj))

(defmethod remove-instance ((obj persisted-entry))
  (let ((revision (persisted-entry/couchdb-revision obj)))
    (unless revision
      (error "No revision value"))
    (let ((id (persisted-entry/couchdb-id obj)))
      (clouchdb:delete-document id :revision revision)
      (flush-cache-for-object-if-enabled (class-of obj) id)
      (run-hooks-for-obj obj :delete))))

(defun remove-instance-nofail (obj)
  "Remove instance OBJ, but don't raise an error if the object doesn't
exist. This function is designed to be exclusively used while
recovering from code that creates multiple couchdb instances.
Therefore, it always issues a warning when the function is called."
  (log:warn "Deleting instance with nofail: ~s" obj)
  (handler-case
      (remove-instance obj)
    (clouchdb:document-missing ()
      (log:trace "Object not available when removing with nofail: ~s" obj))))

(defgeneric save-attachment (obj name content &key content-type))

(defmethod save-attachment ((obj persisted-entry) name content &key content-type)
  (let ((class (class-of obj)))
    (unless (persisted-entry-class/attachments-p class)
      (error "Attempt to save attachment for an object which does not have attachment enabled"))
    (let* ((id (persisted-entry/couchdb-id obj))
           (result (clouchdb:add-attachment id content
                                            :name name
                                            :content-type content-type)))
      (run-hooks-for-obj obj :save-attachment)
      (flush-cache-for-object-if-enabled class id)
      result)))

(defgeneric load-attachment (obj name &key force-binary))

(defmethod load-attachment ((obj persisted-entry) name &key (force-binary t))
  (let ((class (class-of obj)))
    (unless (persisted-entry-class/attachments-p class)
      (error "Attempt to load attachment for an object which does not have attachment enabled"))
    (clouchdb:get-attachment-stream (persisted-entry/couchdb-id obj) name :force-binary force-binary)))

(defmacro define-hook-fn (name class (obj &key (type :save)) &body body)
  "Define a hook function NAME for CLASS."
  (alexandria:with-gensyms (class-sym obj-sym type-sym)
    (multiple-value-bind (rem-forms declarations doc-string)
        (alexandria:parse-body body :documentation t)
      `(progn ;;eval-when (:compile-toplevel :load-toplevel :execute)
         (let ((,class-sym (find-class ',class)))
           (defun ,name (,obj-sym ,type-sym)
             ,@(if doc-string (list doc-string))
             (when ,(cond ((symbolp type)
                           `(eq ,type-sym ,type))
                          ((alexandria:sequence-of-length-p type 1)
                           `(eq ,type-sym ,(car type)))
                          (t
                           `(member ,type-sym ',type)))
               (let ((,obj ,obj-sym))
                 ,@declarations
                 ,@rem-forms)))
           (pushnew ',name (persisted-entry-class/hooks ,class-sym)))))))

(defun call-clouchdb-update-function (category name document params)
  (loop
     with max-repeat-attempts = 25

     ;; Progressively increase the delay between attempts
     for delay = 1 then 2

     ;; Attempt the update
     for result = (clouchdb::db-request (concatenate 'string
                                                     (clouchdb::url-encode (clouchdb:db-name clouchdb:*couchdb*))
                                                     "/_design/"
                                                     (clouchdb::url-encode category)
                                                     "/_update/"
                                                     (clouchdb::url-encode name)
                                                     "/"
                                                     (clouchdb::url-encode document))
                                        :method :put
                                        :parameters params)

     ;; Some update functions does not return a list, but all error functions do.
     ;; Thus, if the result is not of type list, then the update was successful.
     for error = (and (listp result)
                      (assoc :|error| result))

     ;; If this fails after 25 attempts, something is seriously wrong
     repeat max-repeat-attempts

     ;; If there was no error just return the result
     unless error
     do (return result)

     ;; If we get here, that means there was an error. We only want to loop if the error
     ;; was of type "conflict", so if it wasn't we'll throw an error.
     unless (equal (cdr error) "conflict")
     do (error "Unable to perform update. category=~s, name=~s, document=~s, params=~s, error=~s"
               category name document params error)
     
     ;; Sleep for a little while before attempting the update again
     do (sleep (/ delay 10))

     ;; If we get here, that means that the maximum number of update attempts have been
     ;; reached. If this is the case, then throw an error.
     finally (error "Failed to perform update after ~a attempts. category=~s, name=~s, document=~s, params=~s"
                    max-repeat-attempts category name document params)))

(defun invoke-view-and-load-instances (class &rest view-args)
  (let ((class (etypecase class
                 (symbol (find-class class))
                 (class class))))
    (let ((result (apply #'clouchdb:invoke-view view-args)))
      (loop
         for row in (getfield :|rows| result)
         collect (load-instance-from-doc class (getfield :|value| row))))))

(defun make-random-couchdb-id ()
  (ironclad:byte-array-to-hex-string (secure-random:bytes 10 secure-random:*generator*)))

(defun call-with-db-entries (fn)
  (loop
     with block-size = 1000
     for prev-timestamp = nil then timestamp
     for timestamp = (local-time:now)
     for index from 0 by block-size
     for first = t then nil
     for result = (clouchdb:get-all-documents :limit block-size :include-docs t)
     then (clouchdb:get-all-documents :limit block-size
                                      :start-key (getfield :|id| (car (last (getfield :|rows| result))))
                                      :include-docs t)
     for rows = (getfield :|rows| result)
     for updated-rows = (if first rows (cdr rows))
     while updated-rows
     do (format t "Processing ~a~@[ (time: ~a)~]~%" index (if prev-timestamp (local-time:timestamp-difference timestamp prev-timestamp)))
     do (loop
           for row in updated-rows
           do (funcall fn (cdr (assoc :|doc| row))))))

