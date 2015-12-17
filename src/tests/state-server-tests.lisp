(in-package :potato-tests)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(define-condition exit-thread (error)
  ()
  (:documentation "Condition that is raised in order to force a thread to exit"))

(defun simulate-state-server-clients (n)
  (loop
     with channel-id = "e59bd938b2d107911f88de8c86156493"
     with users = (loop
                     for i from 0 below n
                     collect (potato.core:load-user-by-email (format nil "u~a@foo.com" i)))
     do (loop
           for u in users
           do (potato.core:signout-user u channel-id))
     do (loop
           for u in users
           do (potato.core:refresh-user u channel-id))
     do (sleep 10)))

(defun simulate-state-server-clients-threaded (start end)
  (let ((channel-id "e59bd938b2d107911f88de8c86156493"))
    (loop
       for i from start below end
       for user = (potato.core:load-user-by-email (format nil "u~a@foo.com" i))
       do (sleep 1/50)
       do (let ((u user))
            (bordeaux-threads:make-thread (lambda ()
                                            (loop
                                               do (potato.core:refresh-user u channel-id)
                                               do (sleep 10)
                                               do (potato.core:signout-user u channel-id))))))))

(define-test idgen-test ()
  (let* ((nthreads 100)
         (nvalues 10000)
         (results (make-array (* nthreads nvalues) :initial-element nil))
         (threads nil)
         (idgen (state-server::make-id-generator)))
    (loop
       repeat nthreads
       for i from 0 by nvalues
       for thread = (let ((index i))
                      (bordeaux-threads:make-thread (lambda ()
                                                      (loop
                                                         repeat nvalues
                                                         for v from index
                                                         for id = (state-server::id-generator/next-value idgen)
                                                         do (setf (aref results v) id)))))
       do (push thread threads))
    (loop
       for thread in threads
       do (bordeaux-threads:join-thread thread))
    (assert-true (loop
                    with sorted = (sort results #'<)
                    with prev = nil
                    for v across sorted
                    when (and prev (>= prev v))
                    return nil
                    do (setq prev v)
                    finally (return t)))))
