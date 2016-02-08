(in-package :potato.api)

(declaim #.potato.common::*compile-decl*)

(define-api-method (register-gcm-token "/register-gcm" nil ())
  (api-case-method
    (:post
     (unless (potato.gcm:gcm-enabled-p)
       (raise-api-error "GCM is not enabled" hunchentoot:+http-service-unavailable+ '("reason" "gcm_disabled")))
     (json-bind ((token "token"))
         (parse-and-check-input-as-json)
       (ecase (potato.gcm:register-gcm (potato.core:current-user) token)
         (:not-changed (st-json:jso "result" "ok" "detail" "already_registered"))
         (:token-updated (st-json:jso "result" "ok" "detail" "token_updated"))
         (:new-registration (st-json:jso "result" "ok" "detail" "token_registered")))))))

(define-api-method (update-unread-subscriptions "/channel/([^/]+)/unread-notification" t (cid))
  (api-case-method
    (:post
     (let ((data (parse-and-check-input-as-json))
           (channel (potato.core:load-channel-with-check cid)))
       (potato.gcm:update-unread-subscription (potato.core:current-user)
                                              (st-json:getjso "token" data)
                                              channel
                                              (st-json:from-json-bool (st-json:getjso "add" data)))
       (st-json:jso "result" "ok")))))
