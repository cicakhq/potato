(defpackage :potato.content-processor.spotify
  (:use :cl :potato :potato.common))

(in-package :potato.content-processor.spotify)

(defvar *client-id* nil)
(defvar *client-secret* nil)

(alexandria:define-constant +token-memcached-key+ "spotify-key"
  :test #'equal
  :documentation "Memcached key for the active spotify key")

(defun encode-secret ()
  (base64:string-to-base64-string (format nil "~a:~a" *client-id* *client-secret*)))

(defun %request-authentication-token ()
  (multiple-value-bind (body code headers orig-url stream should-close reason)
      (drakma:http-request "https://accounts.spotify.com/api/token"
                           :method :post
                           :parameters '(("grant_type" . "client_credentials"))
                           :additional-headers `(("Authorization" . ,(format nil "Basic ~a" (encode-secret))))
                           :want-stream t)
    (declare (ignore body headers orig-url))
    (unwind-protect
             (if (/= code hunchentoot:+http-ok+)
                 (log:warn "Error getting authentication token. code=~s, reason=~s" code reason)
                 (let ((result (st-json:read-json stream)))
                   (let ((access-token (st-json:getjso "access_token" result))
                         (expires-in (st-json:getjso "expires_in" result)))
                     (list access-token expires-in))))
      (when should-close (close stream)))))

(defun request-authentication-token ()
  (let ((result (cl-memcached:mc-get (list +token-memcached-key+))))
    (if result
        (babel:octets-to-string (fifth (car result)) :encoding :utf-8)
        (destructuring-bind (token expires)
            (%request-authentication-token)
          (cl-memcached:mc-set +token-memcached-key+ token :timeout (/ expires 5))
          token))))

(defun track-info (track-id)
  (multiple-value-bind (body code headers orig-url stream should-close reason)
      (drakma:http-request (format nil "https://api.spotify.com/v1/tracks/~a" track-id)
                           :want-stream t)
    (unwind-protect
         (if (/= code hunchentoot:+http-ok+)
             (log:warn "Error getting track info. code=~s, reason=~s" code reason)
             (st-json:read-json stream))
      (when should-close (close stream)))))

(defun process/spotify (matches message update-html-callback-fn)
  (declare (ignore message))
  )
