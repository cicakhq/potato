(defpackage :potato.content-processor.xkcd
  (:use :cl :potato :potato.common))

(in-package :potato.content-processor.xkcd)

(defun format-xkcd-content (data)
  (let ((safe-title (st-json:getjso "safe_title" data))
        (img (st-json:getjso "img" data)))
    (when (and safe-title img)
      (babel:octets-to-string
       (flexi-streams:with-output-to-sequence (s)
         (lofn:show-template s "xkcd-data.tmpl" `((:title . ,safe-title)
                                                  (:img . ,img))))))))

(defun process/xkcd (matches message update-html-callback-fn)
  (log:trace "Got xkcd link, id=~s, msgkid=~s" (aref matches 0) (st-json:getjso "id" message))
  (multiple-value-bind (body code headers orig-url stream should-close reason)
      (drakma:http-request (format nil "http://xkcd.com/~a/info.0.json" (aref matches 0)) :want-stream t)
    (declare (ignore body headers orig-url))
    (unwind-protect
         (if (= code hunchentoot:+http-ok+)
             (let* ((response-json (st-json:read-json stream))
                    (content (format-xkcd-content response-json)))
               (when content
                 (funcall update-html-callback-fn content)))
             ;; ELSE: Request failed
             (log:trace "xkcd api request failed. code=~s, reason=~s" code reason))
      (when should-close (close stream)))))

(potato.content-processor:register-url-processor 'process/xkcd '("^https?://xkcd.com/([0-9]+)/$"))
