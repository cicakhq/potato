(in-package :potato-client-clim)

(define-condition stop-notification ()
  ())

(defclass notification-reader-state ()
  ((conn             :type potato-client:connection
                     :initarg :connection
                     :reader notification-reader-state/connection)
   (stopped-p        :type dhs-sequences:cas-wrapper
                     :initform (dhs-sequences:make-cas-wrapper nil)
                     :reader notification-reader-state/stopped-p)
   (thread           :accessor notification-reader-state/thread)
   (message-callback :type (or null function)
                     :initform nil
                     :initarg :message-callback
                     :reader notification-reader-state/message-callback)
   (state-callback   :type (or null function)
                     :initform nil
                     :initarg :state-callback
                     :reader notification-reader-state/state-callback)))

(defun call-notification-callback (fn arg)
  (when fn
    (funcall fn arg)))

(defun process-message-event (state event)
  (let ((msg (make-instance 'message
                            :id (st-json:getjso "id" event)
                            :channel (st-json:getjso "channel" event)
                            :from (st-json:getjso "from" event)
                            :from-name (st-json:getjso "from_name" event)
                            :created-date (parse-timestamp (st-json:getjso "created_date" event))
                            :text (parse-text-content (st-json:getjso "text" event)))))
    (log:trace "Created message: ~s" msg)
    (call-notification-callback (notification-reader-state/message-callback state) msg)))

(defun process-state-notification-event (state event)
  (let ((add-type (string-case:string-case ((st-json:getjso "add-type" event))
                    ("sync" :sync)
                    ("add" :add)
                    ("remove" :remove))))
    (call-notification-callback (notification-reader-state/state-callback state)
                                (list add-type
                                      (st-json:getjso "channel" event)
                                      (if (eq add-type :sync)
                                          (mapcar (lambda (v) (st-json:getjso "id" v)) (st-json:getjso "users" event))
                                          (list (st-json:getjso "user" event)))))))

(defun process-incoming-event (state event)
  (log:trace "Incoming event: ~s" event)
  (let ((type (st-json:getjso "type" event)))
    (string-case:string-case (type)
      ("m" (process-message-event state (st-json:getjso "c" event)))
      ("cu" (process-state-notification-event state event))
      (t (log:warn "Unknown event type: ~s" type)))))

(defun start-notifications (conn &key message-callback state-callback)
  (let* ((state (make-instance 'notification-reader-state
                               :connection conn
                               :message-callback message-callback
                               :state-callback state-callback))
         (thread (bordeaux-threads:make-thread (lambda ()
                                                 (notification-reader-loop state))
                                               :name "Notification reader loop")))
    (setf (notification-reader-state/thread state) thread)
    state))

(defun stop-notifications (state)
  (let ((stopped-p (notification-reader-state/stopped-p state)))
    (unless (dhs-sequences:cas-wrapper/value stopped-p)
      (bordeaux-threads:interrupt-thread (notification-reader-state/thread state)
                                         (lambda ()
                                           (signal 'stop-notification)))
      (dhs-sequences:cas stopped-p nil t))))

(defun notification-reader-loop (state)
  (let ((conn (notification-reader-state/connection state))
        (stopped-p (notification-reader-state/stopped-p state)))
    (handler-case
        (potato-client:listener-loop conn
                                     nil
                                     (lambda (event)
                                       (when (dhs-sequences:cas-wrapper/value stopped-p)
                                         (signal 'stop-notification))
                                       (process-incoming-event state event)))
      (stop-notification ()
        (log:debug "Notification reader loop stopped")))))
