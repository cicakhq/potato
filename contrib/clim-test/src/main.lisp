(defpackage :clim-test
  (:use :cl))

(in-package :clim-test)

(clim:define-application-frame potato-frame ()
  ()
  (:panes (channel-list :application)
          (channel-content :application))
  (:layouts (default (clim:horizontally ()
                       (2/10 channel-list)
                       (8/10 channel-content)))))

(defun potato-client-clim ()
  (let ((frame (clim:make-application-frame 'potato-frame :width 500 :height 700)))
    (defparameter *frame* frame)
    (clim:run-frame-top-level frame)))
