(defpackage :potato.web.admin
  (:use :cl :potato :potato.common))

(in-package :potato.web.admin)

(declaim #.potato.common::*compile-decl*)

(defun show-admin-screen (domain-id dev)
  (potato.core:with-authenticated-user ()
    (let ((domain (potato.core:load-domain-with-check domain-id (potato.core:current-user))))
      (lofn:show-template-stream "admin.tmpl" `((:domain-id . ,(potato.core:domain/id domain))
                                                (:dev . ,dev))))))

(potato.core:define-handler-fn-login (admin-screen "/admin/([a-z0-9]+)" t (domain-id))
  (show-admin-screen domain-id nil))

(potato.core:define-handler-fn-login (admin-dev-screen "/admin-dev/([a-z0-9]+)" t (domain-id))
  (show-admin-screen domain-id t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Group admin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(potato.core:define-json-handler-fn-login (admin-find-group-list-screen "/admin/find-group-list" data nil ())
  "Web call that returns a list of all groups in a given domain for which the current user has admin rights."
  (potato.core:with-authenticated-user ()
    (let* ((domain-id (st-json:getjso "domain" data))
           (domain (potato.core:load-domain-with-check domain-id (potato.core:current-user)))
           (result (clouchdb:invoke-view "group" "groups_for_user"
                                         :key (list (potato.core:domain/id domain)
                                                    (potato.core:user/id (potato.core:current-user))
                                                    potato.core:+GROUP-USER-TYPE-ADMIN+))))
      (st-json:jso "groups" (mapcar (lambda (v)
                                      (let ((value (getfield :|value| v)))
                                        (st-json:jso "id" (getfield :|group| value)
                                                     "description" (getfield :|group_name| value))))
                                    (getfield :|rows| result))))))

(defun user-list-as-json (users)
  (let ((descriptions (potato.core:find-descriptions-for-users (mapcar (lambda (v) (getfield :|user_id| v)) users))))
    (loop
       for row in users
       for description in descriptions
       collect (st-json:jso "id" (getfield :|user_id| row)
                            "role" (getfield :|role| row)
                            "description" description))))

(defun parse-api-role-string (role-param)
  (string-case:string-case (role-param)
    ("USER" potato.core:+GROUP-USER-TYPE-USER+)
    ("ADMIN" potato.core:+GROUP-USER-TYPE-ADMIN+)))

(potato.core:define-json-handler-fn-login (admin-load-group-screen "/admin/load-group" data nil ())
  (potato.core:with-authenticated-user ()
    (let* ((gid (st-json:getjso "group" data))
           (group (potato.core:load-group-with-check gid :require-admin-p t))
           (channels (potato.core:find-channels-for-group group)))
      (st-json:jso "group" (st-json:jso "id" (potato.core:group/id group)
                                        "description" (potato.core:group/name group)
                                        "domain" (potato.core:group/domain group)
                                        "users" (user-list-as-json (potato.core:group/users group))
                                        "channels" (mapcar (lambda (channel)
                                                             (st-json:jso "id" (potato.core:channel/id channel)
                                                                          "name" (potato.core:channel/name channel)))
                                                           channels))))))

(potato.core:define-json-handler-fn-login (admin-update-group-screen "/admin/update-group" data nil ())
  (potato.core:with-authenticated-user ()
    (let* ((gid (st-json:getjso "group" data))
           (group (potato.core:load-group-with-check gid :require-admin-p t)))
      (string-case:string-case ((st-json:getjso "cmd" data))
        ("add-user" (potato.core:add-user-to-group group (st-json:getjso "user" data)
                                                   (let ((role-param (st-json:getjso "role" data)))
                                                     (if role-param
                                                         (parse-api-role-string role-param)
                                                         potato.core:+GROUP-USER-TYPE-USER+))))
        ("remove-user" (let ((uid (st-json:getjso "user" data)))
                         (if (equal (potato.core:user/id (potato.core:current-user)) uid)
                             (potato.core:raise-web-parameter-error "A user can't remove itself from a group")
                             (potato.core:remove-user-from-group group uid))))
        ("update-role" (let ((uid (st-json:getjso "user" data))
                             (role-param (st-json:getjso "role" data)))
                         (if (equal (potato.core:user/id (potato.core:current-user)) uid)
                             (potato.core:raise-web-parameter-error "A user can't remove its own admin privileges")
                             (potato.core:update-role-for-group-user group uid (parse-api-role-string role-param))))))
      (st-json:jso "result" "ok"))))

(potato.core:define-json-handler-fn-login (admin-create-channel-screen "/admin/create-channel" data nil ())
  (potato.core:with-authenticated-user ()
    (let* ((gid (st-json:getjso "group" data))
           (name (st-json:getjso "name" data))
           (group (potato.core:load-group-with-check gid :require-admin-p t)))
      (unless (and (stringp name)
                   (plusp (length name)))
        (potato.core:raise-web-parameter-error "Name must be specified and cannot be blank"))
      (let ((channel (potato.core:create-channel name group nil)))
        (st-json:jso "result" "ok"
                     "id" (potato.core:channel/id channel)
                     "name" (potato.core:channel/name channel))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Channel admin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+nil(potato.core:define-json-handler-fn-login (admin-channel-list-screen "/admin/find-channel-list" data nil ())
  (potato.core:with-authenticated-user ()
    (let* ((domain (potato.core:load-domain-with-check (st-json:getjso "domain" data) (potato.core:current-user)))
           (role (potato.core:user-is-in-domain-p domain (potato.core:current-user)))
           (domain-id (potato.core:domain/id domain))
           (uid (potato.core:user/id (potato.core:current-user)))
           (all-channels (potato.db:invoke-view-and-load-instances 'potato.core:channel "channel" "channels_in_domain"))
           (groups (clouchdb:invoke-view "group" "groups_for_user"
                                         :start-key (list domain-id uid nil)
                                         :end-key (list domain-id uid 'clouchdb:json-map))))
      (loop
         for channel in all-channels
         when (or (eq role :admin)
                  (member (potato.core:channel/group channel) groups :key #'potato.core:group/id :test #'equal))
         collect (st-json:jso "id" (potato.core:channel/id channel)
                              "name" (potato.core:channel/name channel))
         into channel-list
         finally (return (st-json:jso "channels" channel-list))))))
