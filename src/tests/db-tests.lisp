(in-package :potato-tests)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defun compare-class-slots (slots ref-obj checked-obj)
  (dolist (slot slots)
    (assert-equal (slot-value ref-obj slot)
                  (slot-value checked-obj slot))))

(defclass serialised-foo ()
  ((name        :type string
                :initarg :name
                :reader serialised-foo/name
                :persisted-p t)
   (description :type (or null string)
                :initarg :description
                :reader serialised-foo/description
                :persisted-p t
                :persisted-name :|description_value|)
   (keyword-value :type keyword
                  :initarg :keyword-value
                  :reader serialised-foo/keyword-value
                  :persisted-p t
                  :persisted-type :symbol)
   (boolean       :type (or null (eql t))
                  :initarg :boolean-value
                  :reader serialised-foo/boolean-value
                  :persisted-p t
                  :persisted-type :boolean))
  (:metaclass potato.db:persisted-entry-class))

(define-test persist-instance-test (:contexts #'couchdb-context :tags '(couchdb db))
  (labels ((save-and-compare (obj)
             (potato.db:save-instance obj)
             (let ((loaded-object (potato.db:load-instance 'serialised-foo (potato.db:persisted-entry/couchdb-id obj))))
               (compare-class-slots '(name description keyword-value boolean) obj loaded-object))))
    (save-and-compare (make-instance 'serialised-foo :couchdb-id "compare1" :name "foo" :description "bar" :keyword-value :foo :boolean-value t))
    (save-and-compare (make-instance 'serialised-foo :couchdb-id "compare2" :name "foo" :description "bar" :keyword-value :foo :boolean-value nil))))

(defclass serialised-bin ()
  ((name         :type string
                 :initarg :name
                 :accessor serialised-bin/name
                 :persisted-p t)
   (binary-value :type (array (unsigned-byte 8) (*))
                 :initarg :binary-value
                 :reader serialised-bin/binary-value
                 :persisted-p nil
                 :documentation "The binary value is not currently used in this test"))
  (:metaclass potato.db:persisted-entry-class)
  (:attachments-p t))

(define-test persist-instance-attachments-test (:contexts #'couchdb-context :tags '(couchdb db))
  (let ((id "attachments-obj"))
    (let ((object (make-instance 'serialised-bin :couchdb-id id :name "somename")))
      (potato.db:save-instance object)
      ;; Add an attachment, reload and resave the object and check that the attachment remains
      (let ((data (make-array 1000 :element-type '(unsigned-byte 8) :initial-element 22)))
        (clouchdb:add-attachment id data :name "image" :content-type "image/jpeg"))
      (let* ((doc (clouchdb:get-document id))
             (attachments (getfield :|_attachments| doc)))
        (assert-true attachments)
        (assert-true (getfield :|image| attachments))
        (let ((loaded-object (potato.db:load-instance 'serialised-bin id)))
          ;; For good measure, let's compare the content
          (compare-class-slots '(name) object loaded-object)
          ;; Save the object again
          (potato.db:save-instance loaded-object)
          (let* ((newdoc (clouchdb:get-document id))
                 (newattachments (getfield :|_attachments| newdoc)))
            (assert-true attachments)
            (assert-true (tree-equal attachments newattachments :test #'equal))))))))

(define-test persist-attachment-helper-test (:contexts #'couchdb-context :tags '(couchdb db))
  (let ((id "attachment-helper-obj"))
    (let ((object (make-instance 'serialised-bin :couchdb-id id :name "somename"))
          (content (make-array 1000 :element-type '(unsigned-byte 8) :initial-element 100)))
      (potato.db:save-instance object)
      (potato.db:save-attachment object "foo-bar" content)
      (let ((loaded-object (potato.db:load-instance 'serialised-bin id)))
        (let ((stream (potato.db:load-attachment loaded-object "foo-bar")))
          (unwind-protect
               (let ((loaded-content (flexi-streams:with-output-to-sequence (output)
                                       (uiop/stream:copy-stream-to-stream stream output :element-type '(unsigned-byte 8)))))
                 (assert-true (compare-arrays content loaded-content)))
            (close stream)))))))

(defclass serialised-list ()
  ((value-list :type list
               :initarg :value-list
               :accessor serialised-list/value-list
               :persisted-p t
               :persisted-type (:list :string)))
  (:metaclass potato.db:persisted-entry-class))

(defun test-serialised-list-instance (name list)
  (let ((obj (make-instance 'serialised-list
                            :couchdb-id name
                            :value-list list)))
    (potato.db:save-instance obj)
    (let ((loaded (potato.db:load-instance 'serialised-list name)))
      (assert-equal list (serialised-list/value-list loaded)))))

(define-test persist-list-test (:contexts #'couchdb-context :tags '(couchdb db))
  (test-serialised-list-instance "instance-1" nil)
  (test-serialised-list-instance "instance-2" '("foo"))
  (test-serialised-list-instance "instance-3" '("foo" "bar"))
  (test-serialised-list-instance "instance-4" '("foo" "bar" "abchellotest"))
  (test-serialised-list-instance "instance-5" '("foo" "bar" "abchellotest" "fooabchello")))

(defclass serialised-list-allow-missing ()
  ((value-list :type list
               :initarg :value-list
               :accessor serialised-list/value-list
               :persisted-p t
               :persisted-type (:list :string)
               :persisted-allow-missing-value t))
  (:metaclass potato.db:persisted-entry-class))

(define-test persist-allow-missing-test (:contexts #'couchdb-context :tags '(couchdb db))
  (let ((id "persist-allow-inst1"))
    (clouchdb:create-document '((:|type| . "serialised_list")) :id id)
    (assert-signal 'potato.db:couchdb-missing-value
                   (potato.db:load-instance 'serialised-list id)))
  (let ((id "persist-allow-inst2"))
    (clouchdb:create-document '((:|type| . "serialised_list_allow_missing")) :id id)
    (potato.db:load-instance 'serialised-list-allow-missing id)))

(defclass serialised-instance-foo ()
  ((value1 :type string
           :initarg :value1
           :accessor serialised-instance-foo/value1
           :persisted-p t
           :persisted-type :string)
   (value2 :type integer
           :initarg :value2
           :accessor serialised-instance-foo/value2
           :persisted-p t
           :persisted-type :integer))
  (:metaclass potato.db:persisted-entry-class))

(defclass serialised-instance-list ()
  ((single :type serialised-instance-foo
           :initarg :single
           :accessor serialised-instance-list/single
           :persisted-p t
           :persisted-type (:inst serialised-instance-foo))
   (list   :type list
           :initarg :list
           :accessor serialised-instance-list/list
           :persisted-p t
           :persisted-type (:list (:inst serialised-instance-foo))))
  (:metaclass potato.db:persisted-entry-class))

(define-test persist-inst-test (:contexts #'couchdb-context :tags '(couchdb db))
  (let ((index 0))
    (labels ((make-foo ()
               (incf index)
               (make-instance 'serialised-instance-foo
                              :value1 (format nil "foo-~a" index)
                              :value2 index))
             (compare-instance (obj1 obj2)
               (assert-equal (serialised-instance-foo/value1 obj1) (serialised-instance-foo/value1 obj2))
               (assert-equal (serialised-instance-foo/value2 obj1) (serialised-instance-foo/value2 obj2))))
      (let* ((id "multi-object")
             (foo (make-foo))
             (foo-list (loop repeat 10 collect (make-foo)))
             (bar (make-instance 'serialised-instance-list
                                 :couchdb-id id
                                 :single foo
                                 :list foo-list)))
        (potato.db:save-instance bar)
        ;; Load and check the result
        (let ((loaded (potato.db:load-instance 'serialised-instance-list id)))
          (assert-true (compare-instance foo (serialised-instance-list/single loaded)))
          (let ((loaded-foolist (serialised-instance-list/list loaded)))
            (assert-equal (length foo-list) (length loaded-foolist))
            (loop
               for a in foo-list
               for b in loaded-foolist
               do (compare-instance a b))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass cached-obj ()
    ((string-value :type string
                   :initarg :string-value
                   :accessor cached-obj/string-value
                   :persisted-p t)
     (number-value :type integer
                   :initarg :number-value
                   :accessor cached-obj/number-value
                   :persisted-p t
                   :persisted-type :integer))
    (:metaclass potato.db:persisted-entry-class)
    (:memcached-enabled-p t)))

(potato.core::define-conspack-encoders cached-obj)

(define-test cached-obj-test (:contexts #'all-context :tags '(couchdb db))
  (labels ((compare-instance (obj1 obj2)
             (assert-equal (cached-obj/string-value obj1) (cached-obj/string-value obj2))
             (assert-equal (cached-obj/number-value obj1) (cached-obj/number-value obj2))))
    (let ((obj (make-instance 'cached-obj :string-value "abc" :number-value 10000)))
      (potato.db:save-instance obj)
      (let* ((id (potato.db:persisted-entry/couchdb-id obj))
             (loaded (potato.db:load-instance 'cached-obj id)))
        (compare-instance obj loaded)
        (setf (cached-obj/string-value loaded) "updated string")
        (setf (cached-obj/number-value loaded) 11)
        (potato.db:save-instance loaded)
        (let ((loaded2 (potato.db:load-instance 'cached-obj id)))
          (compare-instance loaded loaded2))))))

(defclass record-changes ()
  ((val1 :type string
         :initarg :val1
         :initform ""
         :accessor record-changes/val1
         :persisted-p t
         :record-changes-p t)
   (val2 :type string
         :initarg :val2
         :initform ""
         :accessor record-changes/val2
         :persisted-p t))
  (:metaclass potato.db:persisted-entry-class))

(define-test record-changes-test (:contexts #'all-context)
  (let ((obj (make-instance 'record-changes :val1 "x" :val2 "y")))
    (fiveam:is-false (potato.db:persisted-entry-is-value-updated obj 'val1))
    (fiveam:is-false (potato.db:persisted-entry-is-value-updated obj 'val2))
    (setf (record-changes/val1 obj) "x2")
    (setf (record-changes/val2 obj) "y2")
    (fiveam:is-true (potato.db:persisted-entry-is-value-updated obj 'val1))
    (fiveam:is-false (potato.db:persisted-entry-is-value-updated obj 'val2))
    (potato.db:persisted-entry-clear-modifications-list obj)
    (fiveam:is-false (potato.db:persisted-entry-is-value-updated obj 'val1))
    (fiveam:is-false (potato.db:persisted-entry-is-value-updated obj 'val2))))

(defclass record-changes2 ()
  ((val1 :type string
         :initarg :val1
         :initform ""
         :accessor record-changes/val1
         :persisted-p t
         :record-changes-p nil)
   (val2 :type string
         :initarg :val2
         :initform ""
         :accessor record-changes/val2
         :persisted-p t))
  (:metaclass potato.db:persisted-entry-class))

(define-test record-changes-no-changes-test (:contexts #'all-context)
  (let ((obj (make-instance 'record-changes2 :val1 "x" :val2 "y")))
    (fiveam:signals error (fiveam:is-false (potato.db:persisted-entry-is-value-updated obj 'val1)))))
