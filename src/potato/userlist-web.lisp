(defpackage :potato.web.userlist
  (:use :cl :potato :potato.common :potato.web))

(in-package :potato.web.userlist)

(declaim #.potato.common::*compile-decl*)

(potato.core:define-handler-fn-login (userlist-test-screen "/userlist_test/([^/]+)" t (domain-id))
  (potato.core:with-authenticated-user ()
    (lofn:show-template-stream "userlist_test.tmpl" `((:domain . ,domain-id)))))

(defun user-search-node->json (node)
  (st-json:jso "id" (value-by-xpath "str[@name='id']/text()" node)
               "description" (value-by-xpath "str[@name='user_description']/text()" node)))

(potato.core:define-json-handler-fn-login (userlist-search-user "/userlist-search" data nil ())
  (potato.core:with-authenticated-user ()
    (let* ((domain-id (st-json:getjso "domain" data))
           (query (st-json:getjso "query" data))
           (domain (potato.core:load-domain-with-check domain-id (potato.core:current-user))))
      (let ((res (cl-solr:query-noparse query
                                        :parameters `(("defType" . "edismax")
                                                      ("df" . "user_description")
                                                      ("f.name.qf" . "user_description")
                                                      ("f.email.qf" . "user_email")
                                                      ("fq" . ,(format nil "potato_type:\"user\" AND user_domain:\"~a\""
                                                                       (potato.core:domain/id domain)))
                                                      ("uf" . "name email")
                                                      ("rows" . "20")))))
        (let* ((response-node (xpath:first-node (xpath:evaluate "/response/result[@name='response']" res)))
               (results (xpath:map-node-set->list #'user-search-node->json (xpath:evaluate "doc" response-node))))
          (st-json:jso "result" "ok"
                       "users" results))))))

(potato.core:define-json-handler-fn-login (channel-members-screen "/members" data nil ())
  (potato.core:with-authenticated-user ()
    (let* ((channel (potato.core:load-channel-with-check (st-json:getjso "channel" data)))
           (result (potato.core:user-descriptions-for-channel-members channel)))
      (st-json:jso "members" (mapcar #'(lambda (v)
                                         (destructuring-bind (id description nickname user-image)
                                             v
                                           (st-json:jso "id" id
                                                        "description" description
                                                        "nickname" nickname
                                                        "image_name" user-image)))
                                     result)))))

(potato.core:define-json-handler-fn-login (user-details-screen "/user_details" data nil ())
  (potato.core:with-authenticated-user ()
    (let ((domain (potato.core:load-domain-with-check (st-json:getjso "domain" data) (potato.core:current-user)))
          (uids (st-json:getjso "ids" data)))
      (unless uids
        (error "List of user ids required"))
      (log:trace "Requesting user details for domain=~s, uids=~s" (potato.core:domain/id domain) uids)
      (st-json:jso "users" (loop
                              for uid in uids
                              when (potato.core:user-is-in-domain-p domain uid)
                              collect (let ((user (potato.core:load-user uid)))
                                        (st-json:jso "id" (potato.core:user/id user)
                                                     "description" (potato.core:user/description user)
                                                     "nickname" (potato.core:user/nickname user)
                                                     "image_name" (potato.user-image:image-url-for-user user))))))))
