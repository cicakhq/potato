(in-package :potato-tests)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(define-test find-private-channel-test (:contexts #'all-context :tags '(private))
  (destructuring-bind (domain users)
      (make-and-save-activated-users-in-domain "Private tests" '(("foo@bar.com" "Foo1")
                                                                 ("bar@bar.com" "Foo2")))
    (let* ((domain-id (potato.core:domain/id domain))
           (u1-id (potato.core:user/id (first users)))
           (u2-id (potato.core:user/id (second users)))
           (channel (potato.private::find-private-channel-for-users domain-id u1-id u2-id))
           (channel-id (potato.core:channel/id channel)))
      (assert-true (potato.core:user-is-in-channel-p channel-id u1-id))
      (assert-true (potato.core:user-is-in-channel-p channel-id u2-id))
      ;; Attempt to find the newly created group and channel
      (let* ((loaded-channel (potato.private::find-private-channel-for-users domain-id u1-id u2-id))
             (loaded-channel-id (potato.core:channel/id loaded-channel)))
        (assert-equal channel-id loaded-channel-id)
        (assert-true (potato.core:user-is-in-channel-p loaded-channel-id u1-id))
        (assert-true (potato.core:user-is-in-channel-p loaded-channel-id u2-id))))))

(define-test private-single-user-test (:contexts #'all-context :tags '(private))
  (destructuring-bind (domain users)
      (make-and-save-activated-users-in-domain "Private tests" '(("foo@bar.com" "Foo1")))
    (let* ((domain-id (potato.core:domain/id domain))
           (u1-id (potato.core:user/id (first users))))
      (assert-signal 'potato.core:web-parameter-error
                     (potato.private::find-private-channel-for-users domain-id u1-id u1-id)))))
