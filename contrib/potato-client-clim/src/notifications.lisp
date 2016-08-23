(in-package :potato-client-clim)

(define-condition stop-notification ()
  ())

(defclass notification-reader-state ()
  ((conn      :type potato-client:connection
              :initarg :connection
              :reader notification-reader-state/connection)
   (stopped-p :type dhs-sequences:cas-wrapper
              :initform (dhs-sequences:make-cas-wrapper nil)
              :reader notification-reader-state/stopped-p)
   (thread    :accessor notification-reader-state/thread)))

(defun start-notifications (conn)
  (let* ((state (make-instance 'notification-reader-state :connection conn))
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
                                       (process-incoming-event event)))
      (stop-notification ()
        (log:debug "Notification reader loop stopped")))))

(defun process-incoming-event (event)
  (log:info "Incoming event: ~s" event))
