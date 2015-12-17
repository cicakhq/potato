(in-package :potato-tests)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defun create-messages (num-messages start-index)
  (loop
     for i from start-index
     repeat num-messages
     for msg = (potato.core:make-message "e59bd938b2d107911f88de8c86156493" "user-foo@bar.com"
                                         (format nil "~a ~a" (potato.core::make-random-name 40) i))
     do (potato.core:save-message msg)
     do (sleep 1/100)))

(defun make-create-message-threads (num-threads num-messages)
  (let ((threads (loop
                    for i from 0 below num-threads
                    collect (bordeaux-threads:make-thread #'(lambda () (create-messages num-messages (* i 1000000)))))))
    (dolist (thread threads)
      (bordeaux-threads:join-thread thread))))
