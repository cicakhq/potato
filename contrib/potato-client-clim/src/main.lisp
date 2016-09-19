(in-package :potato-client-clim)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defparameter *channel-list-background* (clim:make-rgb-color 0 0 0))
(defparameter *channel-list-foreground* (clim:make-rgb-color 1 1 1))
(defparameter *channel-list-selected-background* (clim:make-rgb-color 0.3 0.43 0.22))
(defparameter *channel-list-selected-foreground* (clim:make-rgb-color 0.9 0.9 0.9))

(defclass message-list (receptacle:sorted-list)
  ()
  (:default-initargs :content (make-instance 'flexichain:standard-flexichain)
                     :test-equal #'equal
                     :test #'string<
                     :key #'message/id))

(defmethod dynlist-size ((list message-list))
  (receptacle:content-length list))

(defmethod dynlist-get ((list message-list) index)
  (receptacle:container-nth list index))

(defclass channel ()
  ((id        :type string
              :initarg :id
              :reader channel/id)
   (name      :type string
              :initarg :name
              :reader channel/name)
   (messages  :type t
              :initform (make-instance 'message-list)
              :reader channel/messages)
   (connected :type (or null (eql t))
              :initform nil
              :accessor channel/connected)
   (users     :type user-db
              :reader channel/users)
   (frame     :type t
              :initarg :frame
              :reader channel/frame)))

(defmethod initialize-instance :after ((obj channel) &key frame)
  (unless frame
    (error "CHANNEL created without :FRAME"))
  (setf (slot-value obj 'users) (make-instance 'user-db
                                               :callback-fn (lambda (users)
                                                              (process-users-updated obj users)))))

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
                   :accessor potato-frame/active-channel)
   (domain         :type string
                   :initarg :domain
                   :reader potato-frame/domain)
   (image-cache    :type image-cache
                   :reader potato-frame/image-cache))
  (:panes (channel-list    :application
                           :default-view (make-instance 'potato-view)
                           :display-function 'display-channel-list
                           :background *channel-list-background*)
          (channel-content :application
                           :default-view (make-instance 'channel-content-view)
                           :display-function 'display-channel-content
                           :scroll-bars :vertical
                           :incremental-redisplay t)
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
                               channel-content
                               input))
                       (2/10 user-list))
                     bottom-adjuster
                     interaction-pane)))

(defmethod clim:frame-standard-output ((frame potato-frame))
  (clim:find-pane-named frame 'channel-content))

(defmethod initialize-instance :after ((obj potato-frame) &key api-key)
  (check-type api-key string)
  (let ((conn (make-instance 'potato-client:connection :api-key api-key)))
    (setf (slot-value obj 'connection) conn)
    (setf (slot-value obj 'image-cache) (make-instance 'image-cache :connection conn))))

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

(clim:define-presentation-to-command-translator select-url
    (url-element open-url-in-browser potato-frame)
    (obj)
  (list (url-element/addr obj)))

(clim:define-presentation-to-command-translator select-user
    (user switch-to-private-channel-frame potato-frame)
    (obj)
  (list obj))

(defun redisplay-channel-content (channel)
  (let* ((frame (channel/frame channel))
         (pane (clim:find-pane-named frame 'channel-content)))
    (when (eq (potato-frame/active-channel frame) channel)
      (clim:redisplay-frame-pane frame pane))))

(defun insert-message-to-channel (channel msg)
  (receptacle:sorted-list-insert (channel/messages channel) msg)
  (redisplay-channel-content channel))

(defun send-message-selected (gadget)
  (declare (ignore gadget))
  (let* ((frame clim:*application-frame*)
         (channel (potato-frame/active-channel frame)))
    (when channel
      (let* ((pane (clim:find-pane-named frame 'input))
             (text (clim:gadget-value pane)))
        (clim:execute-frame-command frame `(send-message ,channel ,text))
        (setf (clim:gadget-value pane) "")))))

(defun make-message-from-json (channel msg)
  (let ((cid (st-json:getjso "channel" msg)))
    (unless (equal (channel/id channel) cid)
      (error "Attempt to create a message for incorrenct channel"))
    (let* ((from (st-json:getjso "from" msg))
           (message (make-instance 'message
                                   :id (st-json:getjso "id" msg)
                                   :channel channel
                                   :from from
                                   :from-name (st-json:getjso "from_name" msg)
                                   :created-date (parse-timestamp (st-json:getjso "created_date" msg))
                                   :text (parse-text-content channel (st-json:getjso "text" msg))
                                   :deleted (eq (st-json:getjso "deleted" msg) :true))))
      (let ((sender (find-user (channel/users channel) from)))
        (find-image-from-url (potato-frame/image-cache (channel/frame channel)) sender
                             (lambda (entry immediate-p)
                               (setf (message/from-image message) (image-cache-entry/pixmap entry))
                               (unless immediate-p
                                 (incf (message/update-index message))
                                 (redisplay-channel-content channel))))
        message))))

(defun load-history-and-update (channel conn)
  (let ((messages (loop
                    with result = (potato-client:message-history (channel/id channel) :connection conn :format "json")
                    for msg-json in (st-json:getjso "messages" result)
                    collect (make-message-from-json channel msg-json))))
    (with-call-in-event-handler (channel/frame channel)
      (dolist (msg messages)
        (insert-message-to-channel channel msg))
      (redisplay-channel-content channel))))

(define-potato-frame-command (switch-to-channel-frame :name "Switch to channel")
    ((obj 'channel))
  (let ((frame clim:*application-frame*))
    (setf (potato-frame/active-channel frame) obj)
    ;; TODO: Should clear and refresh the output record here
    (unless (channel/connected obj)
      (setf (channel/connected obj) t)
      (let ((conn (potato-frame/connection clim:*application-frame*))
            (cid (channel/id obj)))
        (lparallel:future
          (potato-client:subscribe-to-channel cid :connection conn)
          (load-history-and-update obj conn)
          (update-users-from-channel (channel/users obj) conn cid))))))

(define-potato-frame-command (send-message :name "Send message")
  ((channel 'channel)
   (text 'string))
  (let ((frame clim:*application-frame*))
    (lparallel:future
      (potato-client:send-message (channel/id channel) text :connection (potato-frame/connection frame)))))

(define-potato-frame-command (switch-to-private-channel-frame :name "Open private conversation")
  ((obj 'user))
  (log:error "Open private conversation not implemented. user = ~s" (user/id obj)))

(define-potato-frame-command (open-url-in-browser :name "Open URL")
  ((url 'string))
  (uiop:run-program (list "xdg-open" url)))

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
    (receptacle:do-container (msg (channel/messages channel))
      (unless (message/deleted msg)
        (clim:updating-output (stream :unique-id msg
                                      :id-test (lambda (a b)
                                                 (log:trace "ID-TEST ~s → ~s" (message/id a) (eq a b))
                                                 (eq a b))
                                      :cache-value (message/cache-key msg)
                                      :cache-test (lambda (a b)
                                                    (log:trace "CACHE-TEST ~s : ~s → ~s" a b (equal a b))
                                                    (equal a b)))
          (clim:present msg 'message :stream stream)
          (format stream "~&"))))))

(defun handle-message-received (frame event)
  (with-call-in-event-handler frame
    (let ((message-id (st-json:getjso "channel" event)))
      (alexandria:when-let ((channel (find-frame-channel-by-id frame message-id)))
        (insert-message-to-channel channel (make-message-from-json channel event))))))

(defun handle-channel-state-update (frame event)
  (with-call-in-event-handler frame
    (log:info "Channel state update: ~s" event)))

(defun process-users-updated (channel users)
  (declare (ignore users))
  (let ((frame (channel/frame channel)))
    (with-call-in-event-handler frame
      (when (eq (potato-frame/active-channel frame) channel)
        (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'user-list))))))

(defvar *frame* nil)

(defun potato-client-clim (api-key domain)
  (unless lparallel:*kernel*
    (setf lparallel:*kernel* (lparallel:make-kernel 10)))
  (let* ((frame (clim:make-application-frame 'potato-frame
                                             :api-key api-key
                                             :domain domain
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
                      with domain = (potato-client:load-domain (potato-frame/domain frame)
                                                               :connection (potato-frame/connection frame)
                                                               :include-channels t)
                      for group in (cdr (assoc :groups domain))
                      append (loop
                               for channel in (cdr (assoc :channels group))
                               for name = (cdr (assoc :name channel))
                               for private = (cdr (assoc :private channel))
                               unless private
                                 collect (make-instance 'channel
                                                        :id (cdr (assoc :id channel))
                                                        :name name
                                                        :frame frame)))))
      (log:trace "Channels loaded: ~s" channels)
      (with-call-in-event-handler frame
        (setf (potato-frame/channels frame) channels)
        (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'channel-list))))))
