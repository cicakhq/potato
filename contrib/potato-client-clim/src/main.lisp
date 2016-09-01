(in-package :potato-client-clim)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defparameter *channel-list-background* (clim:make-rgb-color 0 0 0))
(defparameter *channel-list-foreground* (clim:make-rgb-color 1 1 1))
(defparameter *channel-list-selected-background* (clim:make-rgb-color 0.3 0.43 0.22))
(defparameter *channel-list-selected-foreground* (clim:make-rgb-color 0.9 0.9 0.9))

(defclass channel ()
  ((id        :type string
              :initarg :id
              :reader channel/id)
   (name      :type string
              :initarg :name
              :reader channel/name)
   (messages  :type dhs-sequences.red-black-tree:red-black-tree
              :initform (make-instance 'dhs-sequences.red-black-tree:red-black-tree
                                       :test (lambda (o1 o2)
                                               (let ((date1 (message/created-date o1))
                                                     (date2 (message/created-date o2)))
                                                 (if (local-time:timestamp= date1 date2)
                                                     (string< (message/id o1) (message/id o2))
                                                     (local-time:timestamp< date1 date2))))
                                       :test-equal (lambda (o1 o2)
                                                     (equal (message/id o1) (message/id o2))))
              :reader channel/messages)
   (connected :type (or null (eql t))
              :initform nil
              :accessor channel/connected)
   (users     :type user-db
              :reader channel/users)))

(defmethod initialize-instance :after ((obj channel) &key frame)
  (unless frame
    (error "CHANNEL created without :FRAME"))
  (setf (slot-value obj 'users) (make-instance 'user-db
                                               :callback-fn (lambda (users)
                                                              (process-users-updated frame obj users )))))

(defmethod print-object ((obj channel) stream)
  (print-unreadable-safely (name messages) obj stream
    (format stream "~s" name)))

(defclass potato-view (clim:view)
  ())

(defclass user-list-view (clim:view)
  ())

(defclass input-view (clim:view)
  ())

(clim:define-application-frame potato-frame ()
  ((connection     :type potato-client:connection
                   :reader potato-frame/connection)
   (channels       :type list
                   :initform nil
                   :accessor potato-frame/channels)
   (active-channel :type (or null channel)
                   :initform nil
                   :accessor potato-frame/active-channel))
  (:panes (channel-list    :application
                           :default-view (make-instance 'potato-view)
                           :display-function 'display-channel-list
                           :background *channel-list-background*)
          (message-content (clim:make-clim-stream-pane :type 'dynlist-pane
                                                       :content #("foo" "bar" "test")
                                                       :default-view (make-instance 'channel-content-view)
                                                       :display-time nil
                                                       :scroll-bars :vertical))
          #+nil(content (clim:make-clim-stream-pane :type 'climacs-flexichain-output-history:flexichain-pane
                                                       :name 'channel-content
                                                       :width 200
                                                       :height 300
                                                       :display-time nil
                                                       :scroll-bars t))
          (user-list       :application
                           :default-view (make-instance 'user-list-view)
                           :display-function 'display-user-list)
          (input           (clim:make-pane 'clim:text-field
                                           :activate-callback #'send-message-selected))
          (bottom-adjuster (clim:make-pane 'clim-extensions:box-adjuster-gadget))
          (interaction-pane :interactor))
  (:layouts (default (clim:horizontally ()
                       (2/10 channel-list)
                       (6/10 (clim:vertically ()
                               message-content
                               input))
                       (2/10 user-list))
                     bottom-adjuster
                     interaction-pane)))

(defmethod clim:frame-standard-output ((frame potato-frame))
  (clim:find-pane-named frame 'channel-content))

(defmethod initialize-instance :after ((obj potato-frame) &key api-key)
  (check-type api-key string)
  (let ((conn (make-instance 'potato-client:connection :api-key api-key)))
    (setf (slot-value obj 'connection) conn)))

(defun find-frame-channel-by-id (frame cid)
  (find cid (potato-frame/channels frame) :key #'channel/id :test #'equal))

(defmethod clim:frame-exit ((frame potato-frame))
  (log:trace "Frame closed: ~s" frame)
  (call-next-method))

(clim:define-presentation-method clim:present (obj (type user) stream (view user-list-view) &key)
  (format stream "~a" (user/description obj)))

(clim:define-presentation-method clim:present (obj (type channel) stream (view potato-view) &key)
  (clim:draw-text* stream (channel/name obj) 10 10))

(clim:define-presentation-method clim:present (obj (type channel) stream (view clim:textual-view) &key)
  (format stream "~a" (channel/name obj)))

(clim:define-presentation-to-command-translator select-channel
    (channel switch-to-channel-frame potato-frame)
    (obj)
  (list obj))

(defun send-message-selected (gadget)
  (declare (ignore gadget))
  (let* ((frame clim:*application-frame*)
         (channel (potato-frame/active-channel frame)))
    (when channel
      (let* ((pane (clim:find-pane-named frame 'input))
             (text (clim:gadget-value pane)))
        (clim:execute-frame-command frame `(send-message ,channel ,text))
        (setf (clim:gadget-value pane) "")))))

(defun load-history-and-update (channel conn frame)
  (loop
    with messages = (potato-client:message-history (channel/id channel) :connection conn :format "json")
    for msg-json in (st-json:getjso "messages" messages)
    for msg = (make-message-from-json msg-json)
    do (dhs-sequences:tree-insert (channel/messages channel) msg))
  (with-call-in-event-handler frame
    (when (eq (potato-frame/active-channel frame) channel)
      (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'channel-content)))))

(define-potato-frame-command (switch-to-channel-frame :name "Switch to channel")
    ((obj 'channel))
  (let ((frame clim:*application-frame*))
    (setf (potato-frame/active-channel frame) obj)
    (unless (channel/connected obj)
      (setf (channel/connected obj) t)
      (let ((conn (potato-frame/connection clim:*application-frame*))
            (cid (channel/id obj)))
        (lparallel:future
          (potato-client:subscribe-to-channel cid :connection conn)
          (load-history-and-update obj conn frame)
          (update-users-from-channel (channel/users obj) conn cid))))))

(define-potato-frame-command (send-message :name "Send message")
  ((channel 'channel)
   (text 'string))
  (let ((frame clim:*application-frame*))
    (lparallel:future
      (potato-client:send-message (channel/id channel) text :connection (potato-frame/connection frame)))))

(defun display-user-list (frame stream)
  (alexandria:when-let ((channel (potato-frame/active-channel frame)))
    (log:info "displaying user list for channel ~s: ~a" channel (user-db/users (channel/users channel)))
    (clim:formatting-table (stream :x-spacing 5 :y-spacing 5)
      (loop
        for user in (users-in-db (channel/users channel))
        do (clim:formatting-row (stream)
             (clim:formatting-cell (stream)
               (present-to-stream user stream)))))))

(defun display-channel-list (frame stream)
  (clim:with-drawing-options (stream :ink *channel-list-foreground*)
   (clim:formatting-table (stream :x-spacing 5 :y-spacing 5)
     (loop
       for channel in (potato-frame/channels frame)
       do (clim:formatting-row (stream)
            (clim:formatting-cell (stream)
              (present-to-stream channel stream)))))))

(defun display-channel-content (frame stream)
  (alexandria:when-let ((channel (potato-frame/active-channel frame)))
    (log:trace "Displaying channel content")
    (loop
      with messages = (channel/messages channel)
      for e = (dhs-sequences:tree-first-node messages) then (dhs-sequences:tree-next messages e)
      while e
      do (let ((msg (dhs-sequences:node-element e)))
           (clim:present msg 'message :stream stream)
           (format stream "~%")))))

(defun handle-message-received (frame msg)
  (with-call-in-event-handler frame
    (alexandria:when-let ((channel (find-frame-channel-by-id frame (message/channel msg))))
      (dhs-sequences:tree-insert (channel/messages channel) msg)
      (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'channel-content)))))

(defun handle-channel-state-update (frame event)
  (with-call-in-event-handler frame
    (log:info "Channel state update: ~s" event)))

(defun process-users-updated (frame channel users)
  (declare (ignore users))
  (with-call-in-event-handler frame
    (when (eq (potato-frame/active-channel frame) channel)
      (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'user-list)))))

(defvar *frame* nil)

(defun potato-client-clim (api-key)
  (unless lparallel:*kernel*
    (setf lparallel:*kernel* (lparallel:make-kernel 10)))
  (let* ((frame (clim:make-application-frame 'potato-frame
                                             :api-key api-key
                                             :width 700 :height 500
                                             :left 10 :top 10))
         (reader (start-notifications (potato-frame/connection frame)
                                      :message-callback (lambda (msg) (handle-message-received frame msg))
                                      :state-callback (lambda (event) (handle-channel-state-update frame event)))))
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
                                                          :name (format nil "~a - ~a" domain-name name)
                                                          :frame frame)))))
      (log:trace "Channels loaded: ~s" channels)
      (with-call-in-event-handler frame
        (setf (potato-frame/channels frame) channels)
        (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'channel-list))))))
