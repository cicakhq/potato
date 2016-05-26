(defpackage :potato.content-processor.github
  (:use :cl :potato :potato.common))

(in-package :potato.content-processor.github)

(defun format-github-project-name (description url homepage owner-avatar-url owner-url)
  (babel:octets-to-string
   (flexi-streams:with-output-to-sequence (s)
     (lofn:show-template s "github-data.tmpl" `((:description . ,description)
                                                (:url . ,url)
                                                (:homepage . ,homepage)
                                                (:owner-avatar-url . ,owner-avatar-url)
                                                (:owner-url . ,owner-url))))))

(defun process/github (matches message update-html-callback-fn)
  (declare (ignore message))
  (let ((user (aref matches 0))
        (project (aref matches 1)))
    (multiple-value-bind (body code headers orig-url stream should-close reason)
        (drakma:http-request (format nil "https://api.github.com/repos/~a/~a" user project) :want-stream t)
      (declare (ignore body headers orig-url reason))
      (unwind-protect
           (when (= code hunchentoot:+http-ok+)
             (flet ((gjso (key data) (nil-if-json-null (st-json:getjso key data))))
               (alexandria:when-let* ((response-json (st-json:read-json stream))
                                      (description (gjso "description" response-json))
                                      (url (gjso "html_url" response-json))
                                      (owner-json (gjso "owner" response-json))
                                      (owner-avatar-url (gjso "avatar_url" owner-json))
                                      (owner-url (gjso "html_url" owner-json)))
                 (funcall update-html-callback-fn (format-github-project-name description url (gjso "homepage" response-json)
                                                                              owner-avatar-url owner-url)))))
        (when should-close (close stream))))))

(potato.content-processor:register-url-processor 'process/github '("^https?://github.com/([a-zA-Z0-9_-]+)/([a-zA-Z0-9_-]+)$"))
