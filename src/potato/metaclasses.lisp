(in-package :potato.db)

(declaim #.potato.common::*compile-decl*)

(defclass persisted-entry ()
  ((couchdb-id          :type (or null string)
                        :initarg :couchdb-id
                        :initform nil
                        :accessor persisted-entry/couchdb-id
                        :documentation "The couchdb id for this instance, or NIL if unknown")
   (couchdb-revision    :type (or null string)
                        :initarg :couchdb-revision
                        :initform nil
                        :accessor persisted-entry/couchdb-revision
                        :documentation "The couchdb revision for this instance, or NIL if unknown")
   (couchdb-attachments :type list
                        :initform nil
                        :accessor persisted-entry/couchdb-attachments
                        :documentation "The content of the _attachments member from couchdb")
   (modifications       :type (or null hash-table)
                        :initform nil
                        :accessor persisted-entry/modifications
                        :documentation "Hashmap that records changes to the object"))
  (:documentation "Superclass for clouchdb persisted instances"))

(defun symbol-to-keyword (symbol)
  (intern (cl-ppcre:regex-replace-all "-" (string-downcase (string symbol)) "_") "KEYWORD"))

(defclass persisted-entry-class (standard-class)
  ((couchdb-type    :type (or null keyword)
                    :initform nil
                    :accessor persisted-entry-class/couchdb-type
                    :documentation "Type name for this class")
   (attachments-p   :type t
                    :initform nil
                    :accessor persisted-entry-class/attachments-p
                    :documentation "If true, the instance will keep the couchdb-attachments member up to date")
   (hooks           :type list
                    :initform nil
                    :accessor persisted-entry-class/hooks
                    :documentation "Hook functions that are called when instances are updated")
   (instance-cached :type t
                    :initform nil
                    :accessor persisted-entry-class/memcached-enabled-p
                    :documentation "When true, instances of this class are cached in memcached"))
  (:documentation "Metaclass for instances persisted using clouchdb"))

(defmethod closer-mop:validate-superclass ((class persisted-entry-class) (superclass standard-object))
  t)

(defclass persisted-entry-class-slot-definition-mixin ()
  ((persisted-field :type keyword
                    :initarg :persisted-name
                    :accessor persisted-entry-class/field
                    :documentation "Key for this field in the couchdb document")
   (persisted-p     :type (or null (eql t))
                    :initarg :persisted-p
                    :accessor persisted-entry-class/persisted-p
                    :documentation "Controls whether the slot is persisted or not")
   (persisted-type  :type (or list keyword)
                    :initform :string
                    :initarg :persisted-type
                    :accessor persisted-entry-class/persisted-slot-type
                    :documentation "Data type for this slot")
   (persisted-allow-missing-value :type (or null (eql t))
                                  :initarg :persisted-allow-missing-value
                                  :initform nil
                                  :accessor persisted-entry-class/persisted-allow-missing-value
                                  :documentation "If T, missing values in a document is interpreted as NIL")
   (persisted-missing-default :type t
                              :initarg :persisted-missing-default
                              :initform nil
                              :accessor persisted-entry-class/persisted-missing-default
                              :documentation "The default value if missing values are allowed")
   (record-changes-p :type t
                     :initarg :record-changes-p
                     :initform nil
                     :accessor persisted-entry-class/record-changes-p
                     :documentation "If T, keep track whether this field have been changed or not"))
  (:documentation "Mixin for slot definition classes"))

(defclass persisted-entry-direct-slot-definition (persisted-entry-class-slot-definition-mixin
                                                  closer-mop:standard-direct-slot-definition)
  ())

(defclass persisted-entry-effective-slot-definition (persisted-entry-class-slot-definition-mixin
                                                     closer-mop:standard-effective-slot-definition)
  ())

(defmethod closer-mop:direct-slot-definition-class ((class persisted-entry-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'persisted-entry-direct-slot-definition))

(defmethod closer-mop:effective-slot-definition-class ((class persisted-entry-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'persisted-entry-effective-slot-definition))

(defun ensure-slot-value (instance field-name &optional default-value)
  "Returns the value of slot FIELD-NAME in INSTANCE. If the slot is unbound, return DEFAULT-VALUE."
  (if (and (slot-exists-p instance field-name)
           (slot-boundp instance field-name))
      (slot-value instance field-name)
      default-value))

(defmethod closer-mop:compute-effective-slot-definition ((class persisted-entry-class) slot-name direct-slots)
  (let ((result (call-next-method)))
    (let ((p (ensure-slot-value (car direct-slots) 'persisted-field)))
      (setf (persisted-entry-class/field result) (cond ((null p)     (symbol-to-keyword slot-name))
                                                       ((keywordp p) p)
                                                       (t            (error "Illegal value for field slot: ~s" p)))))
    (setf (persisted-entry-class/persisted-p result) (ensure-slot-value (car direct-slots) 'persisted-p nil))
    (setf (persisted-entry-class/persisted-slot-type result) (ensure-slot-value (car direct-slots) 'persisted-type :string))
    (setf (persisted-entry-class/persisted-allow-missing-value result) (ensure-slot-value (car direct-slots) 'persisted-allow-missing-value nil))
    (setf (persisted-entry-class/persisted-missing-default result) (ensure-slot-value (car direct-slots) 'persisted-missing-default nil))
    (setf (persisted-entry-class/record-changes-p result) (ensure-slot-value (car direct-slots) 'record-changes-p))
    result))

(defun remove-keyword-from-list (arg-list keyword)
  (when arg-list
    (nconc (unless (eq (car arg-list) keyword)
             (list (car arg-list) (cadr arg-list)))
           (remove-keyword-from-list (cddr arg-list) keyword))))

(macrolet ((init-reinit (name)
             `(defmethod ,name :around ((class persisted-entry-class)
                                        &rest args
                                        &key couchdb-type attachments-p hooks direct-superclasses memcached-enabled-p)
                         (let ((root-class (find-class 'persisted-entry)))
                           (cond ((or (equal class root-class)
                                      (member root-class direct-superclasses))
                                  (call-next-method))
                                 (t
                                  (apply #'call-next-method class
                                         :direct-superclasses (append (list root-class)
                                                                      direct-superclasses)
                                         (remove-keyword-from-list args :direct-superclasses)))))
                         (setf (persisted-entry-class/couchdb-type class)
                               (if couchdb-type
                                   (car couchdb-type)
                                   (symbol-to-keyword (class-name class))))
                         (setf (persisted-entry-class/attachments-p class) attachments-p)
                         (setf (persisted-entry-class/hooks class) hooks)
                         (setf (persisted-entry-class/memcached-enabled-p class) memcached-enabled-p))))
  (init-reinit initialize-instance)
  (init-reinit reinitialize-instance))

(defgeneric couchdb-type-for-class (class)
  (:method ((class symbol))
    (couchdb-type-for-class (find-class class)))
  (:method ((class persisted-entry-class))
    (persisted-entry-class/couchdb-type class)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modification handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun should-record-modifications-p (class)
  (loop
     for slot in (closer-mop:class-slots class)
     when (persisted-entry-class/record-changes-p slot)
     return t
     finally (return nil)))

(defmethod initialize-instance :after ((obj persisted-entry) &key)
  ;; If the class has any slots which needs to record changes, create a tracker for it
  (let ((should-record (should-record-modifications-p (class-of obj))))
    (log:trace "Should record: ~s" should-record)
    (when should-record
      (setf (persisted-entry/modifications obj) (make-hash-table)))))

#+XXX(defmethod closer-mop:compute-slots ((class persisted-entry-class))
  (let ((result (call-next-method)))
    (log:info "Computing slots, result: ~s" result)
    result))

(defmethod (setf closer-mop:slot-value-using-class) :before (new-value (class persisted-entry-class) obj col)
  (when (and (persisted-entry-class/record-changes-p col)
             #+nil(slot-boundp obj 'modifications)
             #+nil(slot-value obj 'modifications)
             (closer-mop:slot-boundp-using-class class obj col))
    (let ((value (closer-mop:slot-value-using-class class obj col)))
      (when (not (equal value new-value))
        (let ((modifications (persisted-entry/modifications obj))
              (n (closer-mop:slot-definition-name col)))
          (multiple-value-bind (v updated-p)
              (gethash n modifications)
            (declare (ignore v))
            (unless updated-p
              (setf (gethash n modifications) value))))))))

(defun persisted-entry-is-value-updated (obj slot)
  (check-type obj persisted-entry)
  (check-type slot symbol)
  (unless (should-record-modifications-p (class-of obj))
    (error "This object does not record changes"))
  (let ((modifications (persisted-entry/modifications obj)))
    (if modifications
        (multiple-value-bind (v updated-p)
            (gethash slot modifications)
          (if updated-p
              (values t v)
              (values nil nil)))
        (values nil nil))))

(defun persisted-entry-clear-modifications-list (obj)
  (check-type obj persisted-entry)
  (let ((modifications (persisted-entry/modifications obj)))
    (when modifications
      (clrhash modifications))))
