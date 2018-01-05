(defpackage :potato.web.domain-welcome
  (:use :cl :potato :potato.common :potato.web))

(in-package :potato.web.domain-welcome)

(declaim #.potato.common::*compile-decl*)

(defun user-is-invited-to-domain-p (domain user)
  (let ((available (potato.core:load-available-domains-for-user user)))
    (member (potato.core:domain/id domain) available :key #'potato.core:domain/id :test #'equal)))

(defun domain-admin-p (domain)
  (eq (potato.core:user-is-in-domain-p domain (potato.core:current-user)) :admin))

(defun make-admin-params-list (domain)
  (let ((domain-invitations-list (potato.core:find-domain-email-invitations-for-domain domain)))
    `((:email-invite-list  . ,(mapcar (lambda (v) `((:email . ,v))) domain-invitations-list))
      (:domain-invite-list . ,(mapcar (lambda (v) `((:domain . ,v))) (potato.core:domain/email-domains domain))))))

(defun make-domain-url (domain domain-nickname)
  (if domain-nickname
    (potato.core:make-potato-url "d/~a" (potato.core:domain-nickname/nickname domain-nickname))
    (potato.core:make-potato-url "domain/~a" (potato.core:domain/id domain))))

(defun make-domain-params (domain)
  (let ((admin-p (domain-admin-p domain))
        (domain-nickname (potato.core:find-domain-nickname-from-domain-id (potato.core:domain/id domain))))
   (append `((:domain-id       . ,(potato.core:domain/id domain))
             (:domain-name     . ,(potato.core:domain/name domain))
             (:domain-nickname . ,(if domain-nickname (potato.core:domain-nickname/nickname domain-nickname)))
             (:new-login       . ,(potato.core:user/new-login (potato.core:current-user)))
             (:domain-admin-p  . ,admin-p)
             (:domain-url      . ,(make-domain-url domain domain-nickname))
             (:listen-url      . ,potato:*external-listen-address*))
           (if admin-p
               (make-admin-params-list domain)
               nil))))

(defun %domain-main-screen (domain-id)
  (potato.core:with-authenticated-user ()
    (let ((user (potato.core:current-user))
          (domain (potato.db:load-instance 'potato.core:domain domain-id)))
      (cond ((potato.core:user-is-in-domain-p domain user)
             (let ((channel-tree (potato.web:make-group-channel-tree-for-user-and-domain domain user :include-private nil)))
               (show-template-stream-with-default-parameters "domain-main.tmpl"
                                                             (append (make-domain-params domain)
                                                                     (list (cons :groups channel-tree))))))
            ((user-is-invited-to-domain-p domain user)
             (show-template-stream-with-default-parameters "domain-invite.tmpl" (make-domain-params domain)))
            (t
             (potato.core:raise-permission-error "No access to domain"))))))

(potato.core:define-handler-fn-login (domain-main-screen "/domain/([^/]+)" t (domain-id))
  (%domain-main-screen domain-id))

(potato.core:define-handler-fn-login (domain-main-nickname-screen "/d/([^/]+)" t (domain-nickname))
  (let ((nick (potato.core:find-domain-nickname domain-nickname)))
    (%domain-main-screen (potato.core:domain-nickname/domain nick))))

(defmacro with-domain ((domain domain-id) &body body)
  (let ((domain-sym (gensym "DOMAIN-")))
    `(let ((,domain-sym (potato.db:load-instance 'potato.core:domain ,domain-id :error-if-not-found nil)))
       (if (and ,domain-sym (domain-admin-p ,domain-sym))
           (let ((,domain ,domain-sym))
             ,@body)
           ;; ELSE: The domain does not exist, or the user does not have admin rights in it
           (error "No admin rights for domain")))))

(potato.core:define-handler-fn-login (domain-invite-email-screen "/domain/([^/]+)/invite_by_email_address" t (domain-id))
  (potato.core:with-authenticated-user ()
    (lofn:case-method
      (:post
       (with-domain (domain domain-id)
         (lofn:with-checked-parameters ((email :name "email" :required t :allow-blank nil :trimmed t))
           (unless (potato.core:is-allowed-email-p email)
             (error "Illegal email address"))
           (potato.workflow:add-email-invitation-for-domain domain email)
           (hunchentoot:redirect (format nil "/domain/~a" (potato.core:domain/id domain)))))))))

(potato.core:define-handler-fn-login (domain-invite-email-domain-screen "/domain/([^/]+)/invite_by_domain" t (domain-id))
  (potato.core:with-authenticated-user ()
    (lofn:case-method
      (:post
       (with-domain (domain domain-id)
         (lofn:with-checked-parameters ((email-domain :name "domain" :required t :allow-blank nil :trimmed t))
           (unless (cl-ppcre:scan "^[a-zA-Z0-9-]+\\.[a-zA-Z0-9-]+$" email-domain)
             (error "Illegal email domain"))
           (unless (find email-domain (potato.core:domain/email-domains domain) :test #'string=)
             (pushnew email-domain (potato.core:domain/email-domains domain) :test #'string=)
             (potato.db:save-instance domain))
           (hunchentoot:redirect (format nil "/domain/~a" (potato.core:domain/id domain)))))))))


(potato.core:define-handler-fn-login (join-domain-screen "/join_domain" nil ())
  (lofn:case-method
    (:post (potato.core:with-authenticated-user ()
             (lofn:with-checked-parameters ((domain-id :name "id" :required t :allow-blank nil))
               (let ((user (potato.core:current-user))
                     (domain (potato.db:load-instance 'potato.core:domain domain-id)))
                 (potato.workflow:add-user-to-domain-with-check domain user)
                 (hunchentoot:redirect (format nil "/domain/~a" (potato.core:domain/id domain)))))))))

(potato.core:define-handler-fn-login (change-domain-nickname-screen "/domain/([^/]+)/change_nickname" t (domain-id))
  (potato.core:with-authenticated-user ()
    (lofn:case-method
      (:post
       (with-domain (domain domain-id)
         (lofn:with-checked-parameters ((nickname :name "nickname" :required t :allow-blank t :trimmed t))
           (let ((nick (if (equal nickname "") nil nickname)))
             (potato.core:set-nickname-for-domain domain nick)
             (hunchentoot:redirect (if nick
                                       (format nil "/d/~a" nick)
                                       (format nil "/domain/~a" (potato.core:domain/id domain)))))))))))
