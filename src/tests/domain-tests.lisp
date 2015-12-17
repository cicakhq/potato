(in-package :potato-tests)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defun domain-mapping-row->user-entry (row)
  (let ((value (getfield :|value| row)))
    (list (getfield :|user| value)
          (intern (getfield :|role| value) "KEYWORD"))))

(defun load-users-in-domain (domain)
  (let ((domain-id (potato.core:ensure-domain-id domain)))
    (let ((result (clouchdb:invoke-view "domain" "users_in_domain" :key domain-id)))
      (mapcar #'domain-mapping-row->user-entry (getfield :|rows| result)))))

(defun domain-tests-make-and-save-user (name &key email password)
  (let* ((user (potato.workflow:register-user (or email (format nil "~a@foo.com" name))
                                              (format nil "User ~a" name)
                                              (or password "foo")
                                              nil
                                              nil)))
    user))

(define-test user-default-domain-test (:contexts #'user-context :tags '(couchdb domain))
  (let ((user (domain-tests-make-and-save-user "user-default-domain")))
    (let ((private-domain (potato.core::load-private-domain-for-user user)))
      (assert-true private-domain)
      (assert-eq :private (potato.core::domain/domain-type private-domain)))))

(define-test add-user-to-privatedomain-test (:contexts #'user-context :tags '(couchdb domain))
  (let ((user1 (domain-tests-make-and-save-user "private-domain-1"))
        (user2 (domain-tests-make-and-save-user "private-domain-2"))
        (user3 (domain-tests-make-and-save-user "private-domain-3")))
    ;; Add user 2 to user 1's domain.
    (potato.workflow::add-user-to-domain user2 (potato.core::load-private-domain-for-user user1) :user)
    ;; Check domain memberships
    (labels ((check-domains (user member-of-domains users-in-private)
               (let* ((domains (potato.core::load-domains-for-user%obsolete user))
                      (private-domain (potato.core::load-private-domain-for-user user))
                      (users-in-domain (load-users-in-domain private-domain)))
                 (assert-equal member-of-domains (length domains))
                 (assert-equal users-in-private (length users-in-domain)))))
      (check-domains user1 1 2)
      (check-domains user2 2 1)
      (check-domains user3 1 1))))

(define-test create-domain-test (:contexts #'user-context :tags '(couchdb domain))
  (let ((user1 (domain-tests-make-and-save-user "user-create-domain-1"))
        (user2 (domain-tests-make-and-save-user "user-create-domain-2"))
        (user3 (domain-tests-make-and-save-user "user-create-domain-3"))
        #+nil(user4 (domain-tests-make-and-save-user "user-create-domain-4")))
    (let ((domain (potato.core::make-and-save-domain "Foo" :corporate)))
      (potato.db:save-instance domain)

      ;; Add users 1, 2 and 3 to the domain
      (let ((id (potato.core::domain/id domain)))
        (potato.workflow::add-user-to-domain user1 id :admin)
        (potato.workflow::add-user-to-domain user2 id :user)
        (potato.workflow::add-user-to-domain user3 id :user)

        ;; Load the user list to confirm that they are members of the domain
        (let ((loaded-users (load-users-in-domain id)))
          (assert-eql 3 (length loaded-users))
          (labels ((check-user-id-loaded-users (user role)
                     (let ((row (find (potato.core:user/id user) loaded-users :key #'first :test #'equal)))
                       (assert-true row)
                       (assert-eq role (second row)))))
            (check-user-id-loaded-users user1 :admin)
            (check-user-id-loaded-users user2 :user)
            (check-user-id-loaded-users user3 :user)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Tests for joining domains
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun is-user-in-domain (user-id domain-id)
  (check-type user-id string)
  (check-type domain-id string)
  (let ((domains (potato.core::load-domains-for-user%obsolete user-id)))
    (member domain-id domains :key #'first :test #'equal)))

(define-test join-domain-via-email-test (:contexts #'user-context :tags '(couchdb domain))
  (let ((user1 (potato.core:user/id (domain-tests-make-and-save-user "user-join-domain-1" :email "a@foo.com")))
        (user2 (potato.core:user/id (domain-tests-make-and-save-user "user-join-domain-2" :email "a@bar.com")))
        (user3 (potato.core:user/id (domain-tests-make-and-save-user "user-join-domain-3" :email "a@blop.com"))))
    (let* ((domain (potato.core::make-and-save-domain "Foo domain" :corporate
                                                      :email-domains '("foo.com" "bar.com"))))
      (potato.db:save-instance domain)
      (let ((domain-id (potato.core::domain/id domain)))
        ;; Add user 1
        (potato.workflow:add-user-to-domain-with-check domain-id user1)
        (assert-true (is-user-in-domain user1 domain-id))
        ;; Add user 2
        (potato.workflow:add-user-to-domain-with-check domain-id user2)
        (assert-true (is-user-in-domain user2 domain-id))
        ;; Add user 3, this should fail since the email domain is wrong
        (assert-signal 'potato.core:permission-error (potato.workflow:add-user-to-domain-with-check domain-id user3))
        (assert-false (is-user-in-domain user3 domain-id))))))
