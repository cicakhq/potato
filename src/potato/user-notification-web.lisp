(defpackage :potato.web.user-notification
  (:use :cl :potato :potato.common :potato.web))

(in-package :potato.web.user-notification)

(declaim #.potato.common::*compile-decl*)

(potato.core:define-json-handler-fn-login (clear-user-notifications-screen "/clear_notification" data nil ())
  (potato.core:with-authenticated-user ()
    (let* ((cid (nil-if-json-null (st-json:getjso "channel" data)))
           (ids (nil-if-json-null (st-json:getjso "ids" data))))
      (log:trace "Clearing notifications. cid=~s, ids=~s" cid ids)
      (if ids
          ;; The authentication check is done inside the couchdb
          ;; update function which will only perform the update if the
          ;; user id matches.
          (potato.user-notification:mark-notifications-by-id (potato.core:current-user) ids)
          ;; ELSE: No ids, so mark the entire channel as read
          (potato.user-notification:mark-notifications-for-user-channel (potato.core:current-user)
                                                                        (potato.core:load-channel-with-check cid)))
      (st-json:jso "result" "ok"))))
