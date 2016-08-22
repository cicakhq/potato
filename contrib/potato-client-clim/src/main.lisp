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
          (channel-content :application)
          (interaction-pane :interactor))
  (:layouts (default (9/10 (clim:vertically ()
                             (clim:horizontally ()
                               (2/10 channel-list)
                               (8/10 channel-content))))
                     (1/10 interaction-pane))))

(clim:define-presentation-method clim:present (obj (type channel) stream view &key)
  (log:info "Calling present method for channel: ~s, stream: ~s" obj stream)
  ;;
  ;; PROBLEM 1:
  ;;
  ;; The below command fails when CLIM is trying to format the command
  ;; for display in the command panel. This is because DRAW-TEXT*
  ;; can't be used on the string stream that is used in
  ;; DREI::PRESENT-ACCEPTABLY-TO-STRING
  ;;
  ;; Note that the string is hardcoded to "xx" because of problem 2
  ;; below.
  ;;
  (clim:draw-text* stream "xx" 10 10)
  ;;
  ;; PROBLEM 2:
  ;;
  ;; To work around problem 1, I'm using PRINC instead of DRAW-TEXT*.
  ;; When I do that, we see that the OBJ parameter is NIL here. That
  ;; should really never happen.
  ;;
  (princ (if obj (channel/name obj) "<NULL>") stream))

(clim:define-presentation-to-command-translator select-channel
    (channel switch-to-channel-frame potato-frame)
    (obj)
  (list obj))

(define-potato-frame-command (switch-to-channel-frame :name "Switch to channel")
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
  (let* ((frame (clim:make-application-frame 'potato-frame :width 700 :height 500 :left 10 :top 10)))
    (setq *frame* frame)
    (clim:run-frame-top-level frame)))
