(in-package :potato-tests)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defmacro with-timer-queue ((queue) &body body)
  (alexandria:with-gensyms (queue-sym)
    `(let ((,queue-sym (potato.common.timer:make-timer-queue)))
       (unwind-protect
            (let ((,queue ,queue-sym))
              ,@body)
         (potato.common.timer:stop-timer-queue ,queue-sym)))))

(define-test timer-test (:contexts #'all-context)
  (with-timer-queue (q)
    (let (a b c d)
      (potato.common.timer:schedule-timer q 1 (lambda () (setq a (potato.common.timer::current-time))))
      (potato.common.timer:schedule-timer q 2 (lambda () (setq b (potato.common.timer::current-time))))
      (potato.common.timer:schedule-timer q 3 (lambda () (setq c (potato.common.timer::current-time))))
      (potato.common.timer:schedule-timer q 4 (lambda () (setq d (potato.common.timer::current-time))))
      (sleep 5)
      (assert-true (< a b c d)))))

(define-test timer-threaded-test (:contexts #'all-context)
  (with-timer-queue (q)
    (let ((n 100)
          (timers-per-thread 10))
      (labels ((schedule-and-reschedule (callback)
                 (let ((list (loop
                                repeat timers-per-thread
                                for timer = (potato.common.timer:schedule-timer q 1 callback)
                                collect timer)))
                   (loop
                      with timers = list
                      repeat 10
                      do (setq timers (mapcar (lambda (timer)
                                                (potato.common.timer:unschedule-timer q timer)
                                                (potato.common.timer:schedule-timer q 1 callback))
                                              timers))
                      do (sleep 1/10)))))
        (let* ((results (make-array n :initial-element 0))
               (threads (loop
                           for i from 0 below n
                           collect (let ((index i))
                                     (bordeaux-threads:make-thread
                                      (lambda ()
                                        (schedule-and-reschedule (lambda ()
                                                                   (incf (aref results index))))))))))
          (dolist (thread threads)
            (bordeaux-threads:join-thread thread))
          (sleep 2)
          (loop
             for v across results
             do (fiveam:is (= timers-per-thread v))))))))

;;;
;;;  This test 
;;;
;;;          0   1   2   3   4   5   6   7   8
;;;   t0 |---s-------r---------------e-----------|
;;;   t1 |-------s-----------e-------------------|
;;;   t2 |---------------s-------r-------r---e---|
;;;

(define-test timer-expire-test (:contexts #'all-context)
  (with-timer-queue (q)
    (let ((result (make-array 3 :initial-element nil))
          (failed nil)
          (start-time (potato.common.timer::current-time)))
      (flet ((make-callback (i)
               (lambda ()
                 (when (not (null (aref result i)))
                   (setq failed (format nil "Result value was not nil: ~a=~a" i (aref result i))))
                 (setf (aref result i) (- (potato.common.timer::current-time) start-time)))))
        (let ((t0 (potato.common.timer:schedule-timer q 4 (make-callback 0))))
          (sleep 1) ; 1
          (let ((t1 (potato.common.timer:schedule-timer q 3 (make-callback 1))))
            (declare (ignore t1))
            (sleep 1) ; 2
            (potato.common.timer:unschedule-timer q t0)
            (setq t0 (potato.common.timer:schedule-timer q 4 (make-callback 0)))
            (sleep 1) ; 3
            (let ((t2 (potato.common.timer:schedule-timer q 3 (make-callback 2))))
              (sleep 1/2) ; 3.5
              (assert-false (aref result 0))
              (assert-false (aref result 1))
              (assert-false (aref result 2))
              (sleep 1) ; 4.5
              (assert-false (aref result 0))
              (assert-true (aref result 1))
              (assert-false (aref result 2))
              (sleep 1/2) ; 5
              (potato.common.timer:unschedule-timer q t2)
              (setq t2 (potato.common.timer:schedule-timer q 3 (make-callback 2)))
              (sleep 1/2) ; 5.5
              (assert-false (aref result 0))
              (assert-true (aref result 1))
              (assert-false (aref result 2))
              (sleep 1) ; 6.5
              (assert-true (aref result 0))
              (assert-true (aref result 1))
              (assert-false (aref result 2))
              (sleep 1/2) ; 7
              (potato.common.timer:unschedule-timer q t2)
              (setq t2 (potato.common.timer:schedule-timer q 1 (make-callback 2)))
              (sleep 1/2) ; 7.5
              (assert-true (aref result 0))
              (assert-true (aref result 1))
              (assert-false (aref result 2))
              (sleep 1) ; 8.5
              (assert-true (aref result 0))
              (assert-true (aref result 1))
              (assert-true (aref result 2))))))
      (assert-false failed)
      (log:info "Result: ~s" result)
      (assert-true (and (aref result 0) (< 5.5 (aref result 0) 6.5)))
      (assert-true (and (aref result 1) (< 3.5 (aref result 1) 4.5)))
      (assert-true (and (aref result 2) (< 7.5 (aref result 2) 8.5))))))
