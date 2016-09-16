;;;
;;;  Example code from beach
;;;

(defpackage #:example-incremental-redisplay
  (:use #:common-lisp))

(in-package #:example-incremental-redisplay)

(defclass model-object ()
  ((%info :initarg :info :accessor info)))

(defclass model ()
  ((%model-objects :initform (make-instance 'flexichain:standard-flexichain)
		   :reader model-objects)))

(clim:define-application-frame incremental-redisplay ()
  ((%model :initform (make-instance 'model) :reader model))
  (:panes (int :interactor :width 500 :height 100
	       :scroll-bars nil)
	  (app :application
	       :width 500 :height 500
	       :scroll-bars nil
	       :display-time :command-loop
	       :display-function 'display-model
	       :incremental-redisplay t))
  (:layouts (:default (clim:vertically ()
			(clim:scrolling (:scroll-bars t) app)
			(clim:scrolling (:scroll-bars :vertical) int)))))

(defun display-model (frame pane)
  (let ((count 0))
    (loop with chain = (model-objects (model frame))
	  for i from 0 below (flexichain:nb-elements chain)
	  for object = (flexichain:element* chain i)
	  do (clim:updating-output (pane
				    :unique-id object
				    :id-test #'eq
				    :cache-value object
				    :cache-test #'eq)
	       (incf count)
	       (format pane "~a~%" (info object))))
    (format *trace-output* "Called FORMAT ~a times~%" count)))

(defun incremental-redisplay ()
  (clim:run-frame-top-level
   (clim:make-application-frame 'incremental-redisplay)))

(define-incremental-redisplay-command (quit :name t) ()
  (clim:frame-exit clim:*application-frame*))

(define-incremental-redisplay-command (inject-objects :name t) ()
  (loop with chain = (model-objects (model clim:*application-frame*))
	for i from 0 below 10000
	for info = (+ (random 1000000) 1000000)
	for object = (make-instance 'model-object :info info)
	do (flexichain:insert* chain i object)))

(define-incremental-redisplay-command (insert :name t) ((position 'integer))
  (let ((object (make-instance 'model-object :info (random 100)))
	(chain (model-objects (model clim:*application-frame*))))
    (flexichain:insert* chain position object)))
