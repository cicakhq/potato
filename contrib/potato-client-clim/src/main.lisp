(in-package :potato-client-clim)

(defclass channel ()
  ((name :type string
         :initarg :name
         :reader channel/name)
   (messages :type array
             :initform (make-array 0 :element-type 'message :adjustable t :fill-pointer 0)
             :reader channel-content/messages)))

(defclass message ()
  ((text :type string
         :initarg :text
         :reader message/text)))

(clim:define-application-frame potato-frame ()
  ((connection     :type potato-client:connection
                   :reader potato-frame/connection)
   (channels       :type list
                   :initform (list (make-instance 'channel :name "Foo")
                                   (make-instance 'channel :name "Bar"))
                   :reader potato-frame/channels)
   (active-channel :type (or null channel-content)
                   :initform nil
                   :accessor potato-frame/active-channel))
  (:panes (channel-list :application
                        :display-function 'display-channel-list)
          (channel-content :application
                           :display-function 'display-channel-content)
          (interaction-pane :interactor))
  (:layouts (default (9/10 (clim:vertically ()
                             (clim:horizontally ()
                               (2/10 channel-list)
                               (8/10 channel-content))))
                     (1/10 interaction-pane))))

(defmethod initialize-instance :after ((obj potato-frame) &key api-key)
  (check-type api-key string)
  (setf (slot-value obj 'connection) (make-instance 'potato-client:connection :api-key api-key)))

(defmethod clim:frame-exit ((frame potato-frame))
  (log:info "Frame closed: ~s" frame)
  (call-next-method))

(clim:define-presentation-method clim:present (obj (type channel) stream view &key)
  (log:info "Calling present method for channel: ~s, stream: ~s" obj stream)
  (clim:draw-text* stream (channel/name obj) 10 10))

(clim:define-presentation-to-command-translator select-channel
    (channel switch-to-channel-frame potato-frame)
    (obj)
  (list obj))

(define-potato-frame-command (switch-to-channel-frame :name "Switch to channel")
    ((obj 'channel))
  (log:info "Switch to channel command called. Type: ~s" (type-of obj))
  (setf (potato-frame/active-channel clim:*application-frame*) obj))

(defun display-channel-list (frame stream)
  (clim:formatting-table (stream :x-spacing 5 :y-spacing 5)
    (loop
      for channel in (potato-frame/channels frame)
      do (clim:formatting-row (stream)
           (clim:formatting-cell (stream)
             (clim:present channel 'channel :stream stream))))))

(defun display-channel-content (frame stream)
  (let ((channel (potato-frame/active-channel frame)))
    (when channel
      (clim:draw-text* stream (format nil "content for channel: ~a " (channel/name channel)) 10 10))))

(defun potato-client-clim (api-key)
  (let* ((frame (clim:make-application-frame 'potato-frame
                                             :api-key api-key
                                             :width 700 :height 500
                                             :left 10 :top 10)))
    (clim:run-frame-top-level frame)))
