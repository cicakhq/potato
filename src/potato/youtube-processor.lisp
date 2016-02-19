(defpackage :potato.content-processor.youtube
  (:use :cl :potato :potato.common)
  (:export #:*youtube-key*))

(in-package :potato.content-processor.youtube)

(declaim #.potato.common::*compile-decl*)

(defvar *youtube-key* nil
  "API key for accessing the Youtube API")

(defun format-youtube-content (name content)
  (let* ((snippet (st-json:getjso "snippet" content))
         (title (st-json:getjso "title" snippet))
         (thumbnail-list (st-json:getjso "thumbnails" snippet))
         (thumbnail (st-json:getjso "medium" thumbnail-list)))
    (babel:octets-to-string
     (flexi-streams:with-output-to-sequence (s)
       (lofn:show-template s "youtube-data.tmpl"
                           (append (list (cons :name name)
                                         (cons :title title))
                                   (if thumbnail
                                       (list (cons :thumbnail-url (st-json:getjso "url" thumbnail))
                                             (cons :thumbnail-width (st-json:getjso "width" thumbnail))
                                             (cons :thumbnail-height (st-json:getjso "height" thumbnail)))
                                       nil))))
     :encoding :utf-8)))

(defun process/youtube (matches message update-html-callback-fn)
  (if *youtube-key*
      (let ((name (aref matches 0)))
        (log:trace "Got youtube link, id=~s, msgid=~s" (aref matches 0) (st-json:getjso "id" message))
        (multiple-value-bind (body code headers orig-url stream should-close reason)
            (drakma:http-request (format nil "https://www.googleapis.com/youtube/v3/videos?key=~a&part=id,snippet&id=~a"
                                         *youtube-key* name)
                                 :want-stream t :force-binary t)
          (declare (ignore body headers orig-url))
          (unwind-protect
               (if (= code hunchentoot:+http-ok+)
                   (let* ((decoded (flexi-streams:make-flexi-stream stream :external-format :utf-8))
                          (response-json (st-json:read-json decoded))
                          (items (st-json:getjso "items" response-json)))
                     (when items
                       (funcall update-html-callback-fn (format-youtube-content name (car items)))))
                   ;; ELSE: Request failed
                   (log:trace "Youtube API request failed. code=~s, reason=~s" code reason))
            (when should-close (close stream)))))
      ;; ELSE: No youtube api key
      (log:warn "No Youtube API key configured")))

(potato.content-processor:register-url-processor 'process/youtube '("^https?://www.youtube.com/watch\\?v=([a-zA-Z0-9_-]+)"
                                                                    "^https?://youtu.be/([a-zA-Z0-9_-]+)"))
