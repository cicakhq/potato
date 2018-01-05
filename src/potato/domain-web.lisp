(defpackage :potato.web.domain
  (:use :cl :potato :potato.common :potato.web))

(in-package :potato.web.domain)

(declaim #.potato.common::*compile-decl*)

(defmacro ensure-domain-create-allowed (&body body)
  `(if potato.web:*allow-create-domain*
       (progn ,@body)
       (error "Domain creation is not enabled")))

(potato.core:define-json-handler-fn-login (available-domains-screen "/available_domains" data nil ())
  (declare (ignore data))
  (potato.core:with-authenticated-user ()
   (let ((domains (potato.core:load-domains-for-user%obsolete (potato.core:current-user)))
         (available (potato.core:load-available-domains-for-user (potato.core:current-user))))
     (st-json:jso "domains" (mapcar (lambda (v)
                                      (destructuring-bind (id name role) v
                                        (st-json:jso "id" id
                                                     "name" name
                                                     "role" (symbol-name role))))
                                    domains)
                  "available_domains" (mapcar (lambda (v)
                                                (st-json:jso "id" (potato.core:domain/id v)
                                                             "name" (potato.core:domain/name v)))
                                              available)))))

(potato.core:define-handler-fn-login (domain-create-screen "/domain_create" nil ())
  (ensure-domain-create-allowed
    (potato.core:with-authenticated-user ()
      (lofn:case-method
        (:get
         (lofn:show-template-stream "domain-create.tmpl" nil))
        (:post
         (lofn:with-checked-parameters ((name :name "name" :required t :allow-blank nil :trimmed t))
           (let ((domain (potato.core:make-and-save-domain name :corporate)))
             (potato.workflow::add-user-to-domain (potato.core:current-user) domain :admin)
             (hunchentoot:redirect (format nil "/domain/~a" (potato.db:persisted-entry/couchdb-id domain))))))))))
