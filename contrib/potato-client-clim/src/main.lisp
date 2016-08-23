(in-package :potato-client-clim)

(defun call-in-event-handler (frame fn)
  (clim:execute-frame-command frame `(funcall ,(lambda () (funcall fn)))))

(defmacro with-call-in-event-handler (frame &body body)
  `(call-in-event-handler ,frame (lambda () ,@body)))

(defclass channel ()
  ((id        :type string
              :initarg :id
              :reader channel/id)
   (name      :type string
              :initarg :name
              :reader channel/name)
   (messages  :type array
              :initform (make-array 0 :element-type 'message :adjustable t :fill-pointer 0)
              :reader channel-content/messages)
   (connected :type (or null (eql t))
              :initform nil
              :accessor channel/connected)))

(defmethod print-object ((obj channel) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~s MESSAGES ~a"
            (slot-value obj 'name)
            (length (slot-value obj 'messages)))))

(defclass message ()
  ((text :type string
         :initarg :text
         :reader message/text)))

(defclass potato-view (clim:view)
  ())

(clim:define-application-frame potato-frame ()
  ((connection     :type potato-client:connection
                   :reader potato-frame/connection)
   (channels       :type list
                   :initform nil
                   :accessor potato-frame/channels)
   (active-channel :type (or null channel-content)
                   :initform nil
                   :accessor potato-frame/active-channel))
  (:panes (channel-list :application
                        :default-view (make-instance 'potato-view)
                        :display-function 'display-channel-list)
          (channel-content :application
                           :display-function 'display-channel-content)
          (interaction-pane :interactor))
  (:layouts (default (9/10 (clim:horizontally ()
                             (2/10 channel-list)
                             (8/10 channel-content)))
                     (1/10 interaction-pane))))

(defmethod initialize-instance :after ((obj potato-frame) &key api-key)
  (check-type api-key string)
  (setf (slot-value obj 'connection) (make-instance 'potato-client:connection :api-key api-key)))

(defmethod clim:frame-exit ((frame potato-frame))
  (log:trace "Frame closed: ~s" frame)
  (call-next-method))

(clim:define-presentation-method clim:present (obj (type channel) stream (view potato-view) &key)
  (log:trace "Calling present method for channel: ~s, stream: ~s" obj stream)
  (clim:draw-text* stream (channel/name obj) 10 10))

(clim:define-presentation-method clim:present (obj (type channel) stream (view clim:textual-view) &key)
  (format stream "~a" (channel/name obj)))

(clim:define-presentation-to-command-translator select-channel
    (channel switch-to-channel-frame potato-frame)
    (obj)
  (list obj))

(define-potato-frame-command (switch-to-channel-frame :name "Switch to channel")
    ((obj 'channel))
  (setf (potato-frame/active-channel clim:*application-frame*) obj)
  (unless (channel/connected obj)
    (setf (channel/connected obj) t)
    (let ((conn (potato-frame/connection clim:*application-frame*)))
      (lparallel:future
        (potato-client:subscribe-to-channel (channel/id obj) :connection conn)))))

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

(defvar *frame* nil)

(defun potato-client-clim (api-key)
  (unless lparallel:*kernel*
    (setf lparallel:*kernel* (lparallel:make-kernel 10)))
  (let* ((frame (clim:make-application-frame 'potato-frame
                                             :api-key api-key
                                             :width 700 :height 500
                                             :left 10 :top 10))
         (reader (start-notifications (potato-frame/connection frame))))
    (unwind-protect
         (progn
           (setq *frame* frame)
           (init-connection frame)
           (clim:run-frame-top-level frame))
      (stop-notifications reader))))

(defun init-connection (frame)  
  (lparallel:future
    (let ((channels (loop
                      with channel-tree = (potato-client:load-channel-tree :connection (potato-frame/connection frame))
                      for domain in channel-tree
                      for domain-name = (cdr (assoc :name domain))
                      for type = (cdr (assoc :type domain))
                      unless (eq type :private)
                        append (loop
                                 for channel in (cdr (assoc :channels domain))
                                 for name = (cdr (assoc :name channel))
                                 for group-type = (cdr (assoc :group-type channel))
                                 unless (eq group-type :private)
                                   collect (make-instance 'channel
                                                          :id (cdr (assoc :id channel))
                                                          :name (format nil "~a - ~a" domain-name name))))))
      (log:trace "Channels loaded: ~s" channels)
      (with-call-in-event-handler frame
        (setf (potato-frame/channels frame) channels)
        (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'channel-list))))))
