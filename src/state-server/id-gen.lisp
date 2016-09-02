(in-package :state-server)

(declaim #.potato.common::*compile-decl*)

(defclass id-generator ()
  ((last-value :type receptacle:cas-wrapper
               :reader id-generator/last-value
               :initform (receptacle:make-cas-wrapper 0))))

(defun make-id-generator ()
  (make-instance 'id-generator))

(declaim (inline idgen-current-time))
(defun idgen-current-time ()
  (let ((now (local-time:now)))
    (+ (* (local-time:timestamp-to-unix now) 1000)
       (truncate (/ (local-time:nsec-of now) 1000000)))))

(defgeneric id-generator/next-value (gen))

(defmethod id-generator/next-value ((gen id-generator))
  (let ((now (idgen-current-time)))
    (loop
       with v = (id-generator/last-value gen)
       for i from 0
       for old = (receptacle:cas-wrapper/value v)
       for n = (+ now i)
       for new = (if (>= old n) (1+ old) n)
       if (eql (receptacle:cas v old new) old)
       return new)))
