(in-package :potato-tests)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(define-test msgl-test (:contexts #'all-context)
  (let ((q (dhs-sequences:make-blocking-queue))
        (m (make-instance 'msgl)))
    (msgl-push m (lambda ()
                   (sleep 1)
                   (dhs-sequences:queue-push q 1)))
    (msgl-push m (lambda ()
                               (sleep 1/2)
                               (dhs-sequences:queue-push q 2)))
    (sleep 2)
    (assert-eql 2 (dhs-sequences:content-length q))
    (let ((v (dhs-sequences:queue-pop q)))
      (assert-eql 1 v))
    (let ((v (dhs-sequences:queue-pop q)))
      (assert-eql 2 v))
    (assert-true (dhs-sequences:empty-p q))))

#+nil(define-test msgl-exception-test (:contexts #'all-context)
  (let ((result nil)
        (m (make-instance 'msgl)))
    (msgl-push m (lambda ()
                               (sleep 1/2)
                               (error "Foo")))
    (msgl-push m (lambda ()
                               (setq result 1)))
    (sleep 2)
    ;; The exception thrown by the first task should cause the msgl
    ;; queue to be cleared, thus preventing the second one from
    ;; running. Thus, the result value should not have been changed.
    (assert-eql nil result)))
