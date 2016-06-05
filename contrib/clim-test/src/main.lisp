(defpackage :clim-test
  (:use :cl))

(clim:define-application-frame potato-frame ()
  ((connection :type connection-state
               :initarg :connection
               :reader potato-frame/connection))
  (:panes (channel-list :application
                        :display-function 'display-channel-list)
          (channel-content :application))
  (:layouts (default (clim:horizontally ()
                       (2/10 channel-list)
                       (8/10 channel-content)))))

(defun display-channel-list (frame stream)
  (let ((conn (potato-frame/connection frame)))
    (clim:formatting-table (stream :x-spacing 5 :y-spacing 5)
      (loop
        for channel in (connection-state/channels conn)
        do (clim:formatting-row (stream)
             (clim:formatting-cell (stream)
               (clim:draw-text* stream (channel/name channel) 10 10)))))))

(defparameter *frame* nil)

(defun potato-client-clim (api-key)
  (let* ((connection (make-instance 'connection-state :api-key api-key))
         (frame (clim:make-application-frame 'potato-frame :connection connection :width 500 :height 700)))
    (setq *frame* frame)
    (clim:run-frame-top-level frame)))
