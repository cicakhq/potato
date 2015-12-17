(in-package :potato.common)

(declaim #.potato.common::*compile-decl*)

;;; This is an implementation fo a very simple sorted list. The
;;; purpose of this object is to have a container type that can hold
;;; the timer queue and is not dependent on a third-party red-black
;;; tree implementation (which have all proven to be broken is various
;;; ways).

(defgeneric sorted-list-insert (list element))
(defgeneric sorted-list-remove (list element))
(defgeneric sorted-list-first-item (list))

(defclass sorted-list-mixin ()
  ((test-fn        :type function
                   :initarg :test
                   :initform #'string<
                   :reader sorted-list/test-fn)
   (test-equal-fn :type function
                  :initarg :test-equal
                  :initform #'string=
                  :reader sorted-list/test-equal-fn)
   (key-fn        :type function
                  :initarg :key
                  :initform #'identity
                  :accessor sorted-list/key-fn)))

(defclass plain-sorted-list (sorted-list-mixin)
  ((content :type list
            :initform nil
            :accessor plain-sorted-list/content)))

(defmethod sorted-list-insert ((list plain-sorted-list) element)
  "Inserts an element into a sorted list."
  (with-accessors ((content plain-sorted-list/content)
                   (test-fn sorted-list/test-fn))
      list
    (if (or (null content)
            (funcall test-fn element (car content)))
        ;; The new element should go in front
        (setf content (cons element content))
        ;; ELSE: The entry should be inserted in the middle
        (loop
           for w on content
           for tail = (cdr w)
           when (or (null tail)
                    (funcall test-fn element (car tail)))
           do (progn
                (setf (cdr w) (cons element tail))
                (return nil))))))

(defmethod sorted-list-remove ((list plain-sorted-list) element)
  "Deletes an entry from a sorted list. Returns the element that was
removed, or NIL if the element could not be found."
  (with-accessors ((content plain-sorted-list/content)
                   (test-equal-fn sorted-list/test-equal-fn))
      list
    (cond ((null content)
           nil)
          ((funcall test-equal-fn element (car content))
           (prog1
               (car content)
             (setf content (cdr content))))
          (t
           (loop
              for w on content
              for tail = (cdr w)
              while tail
              when (funcall test-equal-fn element (car tail))
              do (let ((old-value (car tail)))
                   (setf (cdr w) (cdr tail))
                   (return old-value))
              finally (return nil))))))

(defmethod sorted-list-first-item ((list plain-sorted-list))
  "Returns the first element in the list, or NIL if the list is empty."
  (car (plain-sorted-list/content list)))

;;;
;;;  cl-containers-based implementation
;;;

(defclass cl-containers-sorted-list (sorted-list-mixin)
  ((content :type cl-containers:red-black-tree
            :reader cl-containers-sorted-list/content)))

(defmethod initialize-instance :after ((obj cl-containers-sorted-list) &key)
  (setf (slot-value obj 'content) (make-instance 'cl-containers:red-black-tree
                                                 :sorter (sorted-list/test-fn obj)
                                                 :test (sorted-list/test-equal-fn obj)
                                                 :key (sorted-list/key-fn obj))))
(defmethod sorted-list-insert ((list cl-containers-sorted-list) element)
  (cl-containers:insert-item (cl-containers-sorted-list/content list) element))

(defmethod sorted-list-remove ((list cl-containers-sorted-list) element)
  (let ((content (cl-containers-sorted-list/content list)))
    (let ((result (cl-containers:delete-item content element)))
      ;; cl-containers returns, for some incredibly odd reason, the
      ;; container object itself if the element could not be found in
      ;; the list, so check for this case and return nil instead.
      (if (and result (not (eq result content)))
          result
          nil))))

(defmethod sorted-list-first-item ((list cl-containers-sorted-list))
  (cl-containers:first-item (cl-containers-sorted-list/content list)))

;;;
;;;  dhs-sequences.red-black-tree version
;;;

(defclass dhs-sequences-sorted-list (sorted-list-mixin)
  ((content :type dhs-sequences.red-black-tree:red-black-tree
            :reader dhs-sequences-sorted-list/content)))

(defmethod initialize-instance :after ((obj dhs-sequences-sorted-list) &key)
  (setf (slot-value obj 'content)
        (make-instance 'dhs-sequences.red-black-tree:red-black-tree
                       :test (sorted-list/test-fn obj)
                       :test-equal (sorted-list/test-equal-fn obj)
                       :key (sorted-list/key-fn obj))))

(defmethod sorted-list-insert ((list dhs-sequences-sorted-list) element)
  (dhs-sequences:tree-insert (dhs-sequences-sorted-list/content list) element))

(defmethod sorted-list-remove ((list dhs-sequences-sorted-list) element)
  (let* ((tree (dhs-sequences-sorted-list/content list))
         (node (dhs-sequences:tree-find-node tree element)))
    (if node
        (progn
          (dhs-sequences:tree-delete-node tree node)
          (dhs-sequences:node-element node))
        (progn
          (log:warn "Attempt to remove node ~s which was not in the tree" element)
          nil))))

(defmethod sorted-list-first-item ((list dhs-sequences-sorted-list))
  (let ((tree (dhs-sequences-sorted-list/content list)))
    (dhs-sequences:tree-first-element tree)))

;;;
;;;  logged-sorted-list
;;;

(defclass logged-sorted-list-mixin ()
  ((name            :type string
                    :initarg :name
                    :reader logged-sorted-list/name)
   (value-formatter :type function
                    :initarg :value-formatter
                    :initform #'identity
                    :reader logged-sorted-list/formatter)
   (stream          :type stream
                    :accessor logged-sorted-list/stream)))

(defmethod initialize-instance :after ((obj logged-sorted-list-mixin) &key)
  (setf (logged-sorted-list/stream obj)
        (open (format nil "/tmp/sorted-list.~a.log" (logged-sorted-list/name obj)) :direction :output)))

(defmethod sorted-list-insert :before ((obj logged-sorted-list-mixin) value)
  (let ((out (logged-sorted-list/stream obj)))
    (format out "~s~%" `(:insert ,(funcall (logged-sorted-list/formatter obj) value)))
    (finish-output out)))

(defmethod sorted-list-remove :before ((obj logged-sorted-list-mixin) value)
  (let ((out (logged-sorted-list/stream obj)))
    (format out "~s~%" `(:remove ,(funcall (logged-sorted-list/formatter obj) value)))
    (finish-output out)))

;;;
;;;  locked-sorted-list
;;;

(defclass locked-sorted-list-mixin ()
  ((lock :initform (bordeaux-threads:make-lock)
         :reader locked-sorted-list/lock)))

(defmethod sorted-list-insert :around ((obj locked-sorted-list-mixin) value)
  (bordeaux-threads:with-lock-held ((locked-sorted-list/lock obj))
    (call-next-method)))

(defmethod sorted-list-remove :around ((obj locked-sorted-list-mixin) value)
  (bordeaux-threads:with-lock-held ((locked-sorted-list/lock obj))
    (call-next-method)))

(defclass logged-cl-containers-sorted-list (locked-sorted-list-mixin logged-sorted-list-mixin cl-containers-sorted-list)
  ())
(defclass logged-dhs-sequences-sorted-list (logged-sorted-list-mixin dhs-sequences-sorted-list)
  ())
