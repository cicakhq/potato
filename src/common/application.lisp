(in-package :potato.common.application)

(declaim #.potato.common::*compile-decl*)

(defvar *components* (make-hash-table))

(define-condition component-missing ()
  ((name :type symbol
         :initarg :name
         :reader component-missing/name))
  (:report (lambda (condition stream)
             (format stream "Component not found: ~s" (component-missing/name condition)))))

(define-condition component-started ()
  ((name :type symbol
         :initarg :name
         :reader component-started/name))
  (:report (lambda (condition stream)
             (format stream "Component is already started: ~s" (component-started/name condition)))))

(defclass component ()
  ((name              :type symbol
                      :initarg :name
                      :reader component/name)
   (dependencies      :type list
                      :initarg :dependencies
                      :reader component/dependencies)
   (initialised-state :type (member :stopped :starting :started)
                      :initform :stopped
                      :accessor component/initialised-state)))

(defmethod print-object ((obj component) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~s DEPENDENCIES ~s INITIALISED-STATE ~s"
            (slot-value obj 'name) (slot-value obj 'dependencies) (slot-value obj 'initialised-state))))

(defgeneric start (component))
(defmethod start ((component component))
  nil)

(defgeneric stop (component))
(defmethod stop (component)
  nil)

(defmacro define-component (name &body control-forms)
  (check-type name symbol)
  (let ((component-sym (gensym))
        (start-form nil)
        (stop-form nil)
        (dependencies nil))
    (dolist (f control-forms)
      (ecase (car f)
        (:start (setq start-form (cdr f)))
        (:stop (setq stop-form (cdr f)))
        (:dependencies (setq dependencies (cdr f)))))
    (unless (and (listp dependencies) (every #'symbolp dependencies))
      (error "Dependencies must be a list of symbols"))
    `(progn
       (defclass ,name (component)
         ())
       (defmethod start ((,component-sym ,name))
         (declare (ignore ,component-sym))
         ,@start-form)
       (defmethod stop ((,component-sym ,name))
         (declare (ignore ,component-sym))
         ,@stop-form)
       (setf (gethash ',name *components*)
             (make-instance ',name :name ',name
                                   :dependencies ',dependencies)))))

(defun find-component (name)
  (let ((entry (gethash name *components*)))
    (unless entry
      (error 'component-missing :name name))
    entry))

(defun start-component (name &key (error-if-started nil))
  (restart-case
      (let ((entry (find-component name)))
        (ecase (component/initialised-state entry)
          (:stopped
           (setf (component/initialised-state entry) :starting)
           (dolist (dep (component/dependencies entry))
             (start-component dep :error-if-started nil))
           (log:trace "Starting component: ~s" name)
           (start entry)
           (setf (component/initialised-state entry) :started)
           t)
          (:starting
           (error "Circular component dependency: ~s" name))
          (:started
           (if error-if-started
               (error 'component-started :name name)
               nil))))
    (ignore-component ()
      :report "Continue without starting")))

(defun component-started-p (name)
  (let ((entry (find-component name)))
    (eq (component/initialised-state entry) :started)))

(defun stop-component (name)
  (let ((entry (find-component name)))
    (ecase (component/initialised-state entry)
      (:stopped
       nil)
      (:started
       ;; Check if any dependent components are already running
       (maphash (lambda (key value)
                  (declare (ignore key))
                  (when (and (member (component/name entry) (component/dependencies value))
                             (not (eq (component/initialised-state value) :stopped)))
                    (error "Unable to stop component ~s. Dependency is not stopped: ~s"
                           (component/name entry) (component/name value))))
                *components*)
       (stop entry)
       (setf (component/initialised-state entry) :stopped)
       t)
      (:starting
       (error "Attempt to stop a component while it is currently being started: ~s" (component/name entry))))))
