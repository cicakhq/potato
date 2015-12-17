(in-package :potato.common)

(declaim #.potato.common::*compile-decl*)

(define-condition msgl-request-stop-queue ()
  ())

(defclass msgl ()
  ((messages     :type dhs-sequences:queue
                 :initform (dhs-sequences:make-queue)
                 :reader msgl/messages)
   (processing-p :type t
                 :initform nil
                 :accessor msgl/processing-p)
   (lock         :type t
                 :initform (bordeaux-threads:make-lock "Incoming message list lock")
                 :reader msgl/lock)
   (stopped      :type t
                 :initform nil
                 :accessor msgl/stopped)))

(defun msgl-stop-queue ()
  (signal 'msgl-request-stop-queue))

(defun msgl-push (queue fn)
  (when (bordeaux-threads:with-lock-held ((msgl/lock queue))
          (when (msgl/stopped queue)
            (error "Queue is stopped: ~s" queue))
          (dhs-sequences:queue-push (msgl/messages queue) fn)
          (if (msgl/processing-p queue)
              nil
              (progn
                (setf (msgl/processing-p queue) t)
                t)))
    (lparallel:future
      (block msgl-finish
        (let ((will-exit nil))
          (labels ((stop ()
                     (dhs-sequences:delete-all (msgl/messages queue))
                     (setf (msgl/stopped queue) nil)))
            (unwind-protect
                 (loop
                    for obj = (bordeaux-threads:with-lock-held ((msgl/lock queue))
                                (let ((result (dhs-sequences:queue-pop (msgl/messages queue)
                                                                       :if-empty nil)))
                                  (unless result
                                    (setf (msgl/processing-p queue) nil)
                                    (setq will-exit t))
                                  result))
                    while obj
                    do (handler-bind ((msgl-request-stop-queue (lambda (condition)
                                                                 (declare (ignore condition))
                                                                 (stop)))
                                      (error (lambda (condition)
                                               (log:error "Error type ~s when processing msgl callback: ~a"
                                                          (type-of condition) condition)
                                               (stop)
                                               (when *debug*
                                                 (invoke-debugger condition))
                                               (return-from msgl-finish nil))))
                         (funcall obj)))
              (unless will-exit
                (bordeaux-threads:with-lock-held ((msgl/lock queue))
                  (unless (msgl/processing-p queue)
                    (log:error "In recovery of msgl loop, processing is not active"))
                  (setf (msgl/processing-p queue) nil))))))))))

(defmacro with-msgl (queue &body body)
  `(msgl-push ,queue (lambda () ,@body)))
