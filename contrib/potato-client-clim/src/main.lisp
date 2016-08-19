(in-package :potato-client-clim)

(defclass channel ()
  ((name :type string
         :initarg :name
         :reader channel/name)))

(clim:define-application-frame potato-frame ()
  ((channels :type list
             :initform (list (make-instance 'channel :name "Foo")
                             (make-instance 'channel :name "Bar"))
             :reader potato-frame/channels))
  (:panes (channel-list :application
                        :display-function 'display-channel-list)
          (channel-content :application))
  (:layouts (default (clim:horizontally ()
                       (2/10 channel-list)
                       (8/10 channel-content)))))

(clim:define-presentation-method clim:present (obj (type channel) stream view &key)
  (log:info "Calling present method for channel: ~s" obj)
  (clim:draw-text* stream (channel/name obj) 10 10))

(clim:define-presentation-to-command-translator select-channel
    (channel switch-to-channel-frame potato-frame)
    (obj)
  (list obj))

(define-potato-frame-command (switch-to-channel-frame :name "Switch to channel frame")
    ((obj 'channel))
  (log:info "Switch to channel command called. Type: ~s" (type-of obj)))

(defun display-channel-list (frame stream)
  (clim:formatting-table (stream :x-spacing 5 :y-spacing 5)
    (loop
      for channel in (potato-frame/channels frame)
      do (clim:formatting-row (stream)
           (clim:formatting-cell (stream)
             (clim:present channel 'channel :stream stream))))))

(defparameter *frame* nil)

(defun potato-client-clim ()
  (let* ((frame (clim:make-application-frame 'potato-frame :width 500 :height 700)))
    (setq *frame* frame)
    (clim:run-frame-top-level frame)))
