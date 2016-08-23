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

(defclass message ()
  ((id   :type string
         :initarg :id
         :reader message/id)
   (text :type t
         :initarg :text
         :reader message/text)))

(defmethod print-object ((obj message) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "ID ~s TEXT ~s"
            (slot-value obj 'id)
            (slot-value obj 'text))))

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
  (log:trace "Incoming event: ~s" event)
  (let ((type (st-json:getjso "type" event)))
    (string-case:string-case (type)
      ("m" (process-message-event (st-json:getjso "c" event)))
      (t (log:warn "Unknown event type: ~s" type)))))

(defun process-message-event (event)
  (let ((msg (make-instance 'message
                            :id (st-json:getjso "id" event)
                            :text (parse-text-content (st-json:getjso "text" event)))))
    (log:info "Created message: ~s" msg)))

(defclass text-element ()
  ())

(defclass formatted-element (text-element)
  ((text :type t
         :initarg :text
         :reader text-element/text)))

(defmethod print-object ((obj formatted-element) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "TEXT ~s" (slot-value obj 'text))))

(defclass paragraph-element (formatted-element) ())
(defclass bold-element (formatted-element) ())
(defclass italics-element (formatted-element) ())
(defclass code-element (formatted-element) ())
(defclass newline-element (text-element) ())

(defun parse-text-content (content)
  (etypecase content
    (string content)
    (list (mapcar #'parse-text-content content))
    (st-json:jso (parse-text-part content))))

(defun parse-text-part (content)
  (let ((type (st-json:getjso "type" content)))
    (labels ((make-element (name)
               (make-instance name :text (parse-text-content (st-json:getjso "e" content)))))
      (string-case:string-case (type)
        ("p" (make-element 'paragraph-element))
        ("b" (make-element 'bold-element))
        ("i" (make-element 'italics-element))
        ("code" (make-element 'code-element))
        ("newline" (make-instance 'newline-element))
        (t (format nil "[unknown type:~a]" type))))))
