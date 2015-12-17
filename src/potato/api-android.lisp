(in-package :potato.api)

(declaim #.potato.common::*compile-decl*)

(define-api-method (api-update-android-gcm-key "/user/gcm-key" nil ())
  (api-case-method
    (:post (let* ((data (parse-and-check-input-as-json))
                  (key (st-json:getjso "key" data)))
             (log:trace "Updating android key: ~s" key)
             (unless key
               (raise-api-error "Missing value for \"key\" tag" hunchentoot:+http-bad-request+))
             ;; Load the user again just to make sure we have the most recent version
             (let* ((uid (potato.core:user/id (potato.core:current-user)))
                    (user (potato.db:load-instance 'potato.core:user uid)))
               (setf (potato.core:user/android-gcm-key user) key)
               (potato.db:save-instance user)
               (st-json:jso "result" "ok"))))))
