(in-package :potato-tests)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(define-test workflow-create-domain-test (:contexts #'all-context :tags '(workflow domain))
  (let* ((domain (potato.core::make-and-save-domain "Foo" :corporate))
         (domain-id (potato.core:domain/id domain)))
    ;; Ensure that the group and channel have been created
    (let ((groups (potato.core:find-groups-in-domain domain-id)))
      (assert-eql 1 (length groups))
      (let ((channels (potato.core:find-channels-for-group (potato.core:group/id (car groups)))))
        (assert-eql 1 (length channels)))
      (let ((default-group (potato.core::find-default-group-in-domain domain-id)))
        (assert-eql :domain-default (potato.core::group/type default-group))))))

(define-test workflow-add-user-to-domain-test (:contexts #'all-context :tags '(workflow domain))
  (let ((domain (potato.core::make-and-save-domain "Foo" :corporate))
        (user1 (make-and-save-activated-user "foo@bar.com" "add-user-to-domain")))
    (potato.workflow::add-user-to-domain user1 domain :user)
    (let ((user-id (potato.core:user/id user1)))
      ;; Check that the user is member of a group
      (let ((result (clouchdb:invoke-view "group" "groups_for_user"
                                          :start-key (list (potato.core:domain/id domain) user-id nil)
                                          :end-key (list (potato.core:domain/id domain) user-id 'clouchdb:json-map))))
        (assert-eql 1 (length (getfield :|rows| result))))
      ;; At this point, the user has not actually joined any channels
      #+nil(let ((channels (potato.core::channels-for-user user-id)))
        (assert-eql 0 (length channels))))))
