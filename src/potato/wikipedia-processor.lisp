(defpackage :potato.content-processor.wikipedia
  (:use :cl :potato :potato.common))

(in-package :potato.content-processor.wikipedia)

(defun find-wikipedia-summary (keyword)
  (multiple-value-bind (body code headers orig-url stream should-close reason)
      (drakma:http-request (format nil "https://en.wikipedia.org/w/api.php?action=query&titles=~a&prop=extracts&exintro=&explaintext=&exsentences=1&format=json" keyword)
                           :want-stream t)
    (declare (ignore body headers orig-url reason))
    (unwind-protect
         (when (= code hunchentoot:+http-ok+)
           (let* ((response-json (st-json:read-json stream))
                  (pages (st-json:getjso* "query.pages" response-json))
                  (entry nil))
             (st-json:mapjso (lambda (key value)
                               (declare (ignore key))
                               (setf entry value))
                             pages)
             (unless entry
               (error "Unexpected result from wikipedia lookup for keyword: ~s" keyword))
             (alexandria:when-let ((title (st-json:getjso "title" entry)))
               (alexandria:when-let ((extract (st-json:getjso "extract" entry)))
                 (list title extract)))))
      (when should-close (close stream)))))

(defun format-wikipedia-summary (result)
  (destructuring-bind (title extract) result
    (babel:octets-to-string
     (flexi-streams:with-output-to-sequence (s)
       (lofn:show-template s "wikipedia-data.tmpl" `((:title . ,title)
                                                     (:extract . ,extract)))))))

(defun process/wikipedia (matches message update-html-callback-fn)
  (declare (ignore message))
  (let ((result (find-wikipedia-summary (aref matches 0))))
    (when result
      (funcall update-html-callback-fn (format-wikipedia-summary result)))))

(potato.content-processor:register-url-processor 'process/wikipedia '("^https?://en.wikipedia.org/wiki/([^?#]+)"))
