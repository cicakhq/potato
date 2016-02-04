(in-package :potato.common.timer)

(declaim #.potato.common::*compile-decl*)

(deftype timer-trigger () '(and real (satisfies plusp)))

(defclass checked-dhs-sequences-sorted-list (potato.common:dhs-sequences-sorted-list)
  ()
  (:documentation "Special version of dhs-sequences-sorted-list which
verifies that removed objects actually exists in the list, and throws
an error if not. This is purely to be used for testing, since its use
in the timer queue has a race condition when a timer expires at the
same time an object is being removed from the list."))

(defmethod potato.common:sorted-list-remove :around ((list checked-dhs-sequences-sorted-list) element)
  (let ((element (call-next-method)))
    (unless element
      (error "Illegal state in timer queue detected. Unable to find object in list: ~s" element))))

(defclass timer ()
  ((index        :type integer
                 :initarg :index
                 :reader timer/index)
   (trigger-time :type timer-trigger
                 :initarg :trigger-time
                 :reader timer/trigger-time)
   (callback     :type function
                 :initarg :callback
                 :initform (error "required value ~s missing" :callback)
                 :reader timer/callback)
   (cancelled    :type dhs-sequences:cas-wrapper
                 :initform (dhs-sequences:make-cas-wrapper nil)
                 :accessor timer/cancelled)))

(defmethod print-object ((obj timer) stream)
  (print-unreadable-safely (index trigger-time cancelled) obj stream
    (format stream "INDEX ~s TRIGGER-TIME ~s CANCELLED ~s" index trigger-time (dhs-sequences:cas-wrapper/value cancelled))))

(defclass timer-queue ()
  ((name          :type string
                  :initarg :name
                  :initform (error "required value missing")
                  :reader timer-queue/name)
   (request-queue :type dhs-sequences:blocking-queue
                  :initform (dhs-sequences:make-blocking-queue)
                  :reader timer-queue/request-queue)
   (timers        :type t
                  :reader timer-queue/timers)
   (index         :type dhs-sequences:cas-wrapper
                  :initform (dhs-sequences:make-cas-wrapper 0)
                  :accessor timer-queue/index)
   (thread        :type t
                  :initform nil
                  :accessor timer-queue/thread)
   (lock          :type t
                  :initform (bordeaux-threads:make-lock "Thread lock for timer queue")
                  :reader timer-queue/lock)))

(defmethod initialize-instance :after ((obj timer-queue) &key)
  (setf (slot-value obj 'timers)
        #+nil(make-instance 'potato.common:cl-containers-sorted-list :test #'timer< :test-equal #'eq :key #'identity)
        #+nil(make-instance 'potato.common::logged-cl-containers-sorted-list
                       :test #'timer< :test-equal #'eq :key #'identity
                       :name (timer-queue/name obj)
                       :value-formatter (lambda (v)
                                          (cons (timer/trigger-time v)
                                                (timer/index v))))
        ;; Fallback that can be used if a bug is found in the red-black implementation
        #+nil(make-instance 'potato.common:plain-sorted-list :test #'timer< :test-equal #'eq :key #'identity)
        ;; Standard sorted list to use in production deployments
        (make-instance 'potato.common:dhs-sequences-sorted-list :test #'timer< :test-equal #'eq :key #'identity)
        ;; Don't use the checked sorted list in production. See the note about the race condition in the class documentation
        #+nil(make-instance 'checked-dhs-sequences-sorted-list :test #'timer< :test-equal #'eq :key #'identity)
        ;; Used to debug possible errors in the red-black implementation
        #+nil(make-instance 'potato.common::logged-dhs-sequences-sorted-list
                       :test #'timer< :test-equal #'eq :key #'identity
                       :name (timer-queue/name obj)
                       :value-formatter (lambda (v)
                                          (cons (timer/trigger-time v)
                                                (timer/index v))))))

(defun timer< (v1 v2)
  (let ((time1 (timer/trigger-time v1))
        (time2 (timer/trigger-time v2)))
    (cond ((= time1 time2)
           (< (timer/index v1) (timer/index v2)))
          (t
           (< time1 time2)))))

(defun timer-thread-loop (mgr)
  (loop
     with timers = (timer-queue/timers mgr)
     with request-queue = (timer-queue/request-queue mgr)
     for now = (current-time)
     for next-expired = (sorted-list-first-item timers)
     if (and next-expired (<= (timer/trigger-time next-expired) now))
     do (progn
          (sorted-list-remove timers next-expired)
          (when (null (dhs-sequences:cas (timer/cancelled next-expired) nil t))
            (funcall (timer/callback next-expired))))
     else
     do (let ((task (dhs-sequences:queue-pop-wait request-queue
                                                  :timeout (if next-expired
                                                               (- (timer/trigger-time next-expired) now)
                                                               ;; ELSE: No timers, wait indefinitely
                                                               nil))))
          (etypecase task
            (function (funcall task))
            (null nil)
            (symbol (ecase task
                      (:stop (return-from timer-thread-loop nil))))))))

(defun increment-index (mgr)
  (dhs-sequences:with-cas-update (v (timer-queue/index mgr))
    (1+ v)))

(defun push-request-to-request-queue (mgr fn)
  (dhs-sequences:queue-push (timer-queue/request-queue mgr) fn))

(defun schedule-timer (mgr time callback)
  (check-type mgr timer-queue)
  (check-type time timer-trigger)
  (check-type callback function)
  (ensure-thread-running mgr)
  (let ((timer (make-instance 'timer
                              :trigger-time (+ (current-time) (rationalize time))
                              :callback callback
                              :index (increment-index mgr))))
    (push-request-to-request-queue mgr
                                   (lambda ()
                                     (sorted-list-insert (timer-queue/timers mgr) timer)))
    timer))

(defun update-timer (mgr timer time)
  (check-type mgr timer-queue)
  (check-type timer timer)
  (check-type time timer-trigger)
  (let ((new-timer (make-instance 'timer
                                  :trigger-time (+ (current-time) (rationalize time))
                                  :callback (timer/callback timer)
                                  :index (increment-index mgr))))
    (push-request-to-request-queue mgr
                                   (lambda ()
                                     (let* ((timers (timer-queue/timers mgr))
                                            (node (sorted-list-remove timers timer)))
                                       (when node
                                         (sorted-list-insert timers new-timer)))))
    new-timer))

(defun unschedule-timer (mgr timer)
  (check-type mgr timer-queue)
  (check-type timer timer)
  (when (null (dhs-sequences:cas (timer/cancelled timer) nil t))
    (push-request-to-request-queue mgr
                                   (lambda ()
                                     (let ((timers (timer-queue/timers mgr)))
                                       (sorted-list-remove timers timer))))))

(defun make-timer-queue (&key (name "Timer"))
  (let ((mgr (make-instance 'timer-queue :name name)))
    mgr))

(defun ensure-thread-running (mgr)
  (bordeaux-threads:with-lock-held ((timer-queue/lock mgr))
    (unless (timer-queue/thread mgr)
      (setf (timer-queue/thread mgr)
            (bordeaux-threads:make-thread (lambda ()
                                            (timer-thread-loop mgr))
                                          :name (format nil "Timer manager thread: ~a" (timer-queue/name mgr)))))))

(defun stop-timer-queue (mgr)
  (check-type mgr timer-queue)
  (push-request-to-request-queue mgr :stop)
  (bordeaux-threads:join-thread (timer-queue/thread mgr)))
