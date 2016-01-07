(in-package :potato-tests)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defvar *tests-kernel* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-test-form (contexts body)
    (cond ((null contexts)
           `(progn ,@body))
          (t
           `(funcall ,contexts (lambda () ,(make-test-form nil body)))))))

(defmacro define-test (name (&key contexts tags) &body body)
  (declare (ignore tags))
  `(fiveam:test ,name
     ,(make-test-form contexts body)))

(defmacro assert-equal (expected value)
  `(fiveam:is (equal ,expected ,value)))

(defmacro assert-eql (expected value)
  `(fiveam:is (eql ,expected ,value)))

(defmacro assert-eq (expected value)
  `(fiveam:is (eq ,expected ,value)))

(defmacro assert-true (value)
  `(fiveam:is-true ,value))

(defmacro assert-false (value)
  `(fiveam:is-false ,value))

(defmacro assert-signal (name &body body)
  `(fiveam:signals ,(if (and (listp name) (eq (car name) 'quote))
                        (cadr name)
                        name)
     ,@body))

(defun compare-arrays (v1 v2)
  (and (= (length v1) (length v2))
       (loop
          for e1 across v1
          for e2 across v2
          unless (eql e1 e2)
          do (return nil)
          finally (return t))))

(defun make-db-and-reset (name)
  (let ((db (clouchdb:make-db :host potato.common:*db-hostname*
                              :port (princ-to-string potato.common:*db-port*)
                              :name name)))
    (let ((clouchdb:*couchdb* db))
      (clouchdb:delete-db :if-missing :ignore))
    db))

(defun couchdb-context (body-fn)
  (let* ((clouchdb:*couchdb* (make-db-and-reset "test-db"))
         (potato.common:*user-notification-db* (make-db-and-reset "test-db-notifications"))
         (potato.common:*messages-db* (make-db-and-reset "test-db-messages")))
    (potato.common.application:start-component 'potato.db::db)
    (unwind-protect
         (funcall body-fn)
      (potato.common.application:stop-component 'potato.db::db))))

(defun memcached-context (body-fn)
  (potato.common.application:start-component 'potato.common.memcached::memcached)
  (cl-memcached:mc-flush-all)
  (funcall body-fn))

(defun rabbitmq-context (body-fn)
  (potato.common.application:start-component 'potato.common::rabbitmq)
  (funcall body-fn))

(defun all-context (body-fn)
  (let ((potato:*smtp-server-host* nil)
        (potato.core::*inhibit-no-smtp-server-warning* t)
        (potato.core::*inhibit-lparallel* t))
    (unless *tests-kernel*
      (setf *tests-kernel* (lparallel:make-kernel 10 :name "lparallel-tests-kernel")))
    (let ((lparallel:*kernel* *tests-kernel*))
      (rabbitmq-context (lambda () (memcached-context (lambda () (couchdb-context body-fn))))))))

(defun user-context (body-fn)
  (all-context (lambda ()
                 (let ((email "foo@foo.com")
                       (id (potato.db:make-random-couchdb-id)))
                   (let ((u (make-instance 'potato.core:user
                                           :couchdb-id (concatenate 'string "user-" id)
                                           :nickname id
                                           :description "Foo"
                                           :activated-p "2014-01-01T12:00:00.0000Z"
                                           :activate-code "")))
                     (potato.core::user/update-password u "foo")
                     (let ((emailuser (make-instance 'potato.core:user-email
                                                     :user (potato.core:user/id u)
                                                     :email email)))
                       (potato.db:save-instance emailuser))
                     (potato.db:save-instance u))
                   (let* ((user (potato.core::load-user-by-email email))
                          (potato.core::*current-auth-user* user))
                     (funcall body-fn))))))

(define-test clouchdb-insert-document (:contexts #'couchdb-context :tags '(couchdb))
  (let ((result (clouchdb:create-document '((:|value| . "foo")
                                            (:|text| . "other text")))))
    (let ((new-doc (clouchdb:get-document (getfield :|id| result))))
      (assert-equal "foo" (getfield :|value| new-doc))
      (assert-equal "other text" (getfield :|text| new-doc)))))

(define-test make-message-id-no-index (:tags '(foo))
  (let ((id (potato.core::make-message-id-no-index "45af51ce6ed9f21cf656082dd93a790d" "2014-11-12T04:16:32.591897Z")))
    (assert-equal "msg-45af51ce6ed9f21cf656082dd93a790d-2014-11-12T04:16:32.591897Z" id)))

(defun run-all-tests ()
  #+nil(lisp-unit2:print-summary (run-tests :package :potato-tests))
  (fiveam:run!))

(defun make-and-save-activated-users-in-domain (domain-name user-list)
  (let ((domain (potato.core::make-and-save-domain domain-name :corporate))
        (users nil))
    (dolist (user-descriptor user-list)
      (let ((user (make-and-save-activated-user (first user-descriptor) (second user-descriptor))))
        (potato.workflow::add-user-to-domain user domain :user)
        (let ((role (potato.core:user-is-in-domain-p domain user)))
          (assert-true (eq role :user)))
        (push user users)))
    (list domain (reverse users))))

(defun make-and-save-unregistered-user (email description password)
  (let ((user (potato.core::make-unregistered-user description password)))
    (let ((emailuser (make-instance 'potato.core:user-email
                                    :user (potato.core:user/id user)
                                    :email email)))
      (potato.db:save-instance emailuser))
    (potato.db:save-instance user)
    user))

(defun make-and-save-activated-user (email description)
  (let ((user (make-and-save-unregistered-user email description "foo")))
    (potato.core::activate-user user)
    user))

(defun bootstrap-debug-env (domain-name channel-names emails)
  (let* ((domain (potato.core::make-and-save-domain domain-name :corporate))
         (group (potato.core:find-default-group-in-domain domain)))
    (log:trace "Domain created: ~a, default group id: ~a" (potato.core:domain/id domain) (potato.core:group/id group))
    (let ((users (loop
                    for email in emails
                    collect (let ((user (make-and-save-activated-user email email)))
                              (potato.core:user/update-password user "a")
                              (potato.db:save-instance user)
                              (potato.workflow::add-user-to-domain user domain :user)
                              (log:trace "User created: ~a" (potato.core:user/id user))
                              user))))
      (loop
         for channel-name in channel-names
         do (let ((channel (potato.core::create-channel channel-name group (mapcar #'potato.core:user/id users))))
              (log:trace "Channel created: ~a" (potato.core:channel/id channel)))))))
