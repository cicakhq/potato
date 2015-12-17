(in-package :potato.common)

(declaim #.potato.common::*compile-decl*)

(defun format-message-custom-parser (string fn)
  (markup-from-regexp #.(let ((ch (code-char #xf0001)))
                          (format nil "~cuser:([a-zA-Z0-9.@-]+):([^~c]*)~c" ch ch ch))
                      string
                      (lambda (reg-starts reg-ends)
                        (list :user
                              (subseq string (aref reg-starts 0) (aref reg-ends 0))
                              (subseq string (aref reg-starts 1) (aref reg-ends 1))))
                      fn))

(defun format-message-custom-render-html (content stream fn)
  (declare (ignore fn))
  (ecase (car content)
    (:user (format stream "<em class=\"user\" user-id=\"~a\">~a</em>"
                   (second content)
                   (with-output-to-string (s)
                     (escape-string (third content) s))))))

(defun markup-message-content (text)
  (let ((cl-markup:*custom-parser-1* #'format-message-custom-parser))
    (cl-markup:markup-paragraphs text :allow-nl t)))

(defun format-message (text)
  (let ((cl-markup:*custom-html-renderer* #'format-message-custom-render-html))
    (with-output-to-string (stream)
      (cl-markup:render-markup-to-stream (markup-message-content text) stream))))

(defun message-contains-math-p (html)
  (cl-ppcre:scan "(?:\\\\\\(.+\\\\\\))|(?:\\$\\$.+\\$\\$)" html))

(defun message-content-as-json (text)
  (labels ((parse-element-list (e)
             (if (stringp e)
                 (list e)
                 (mapcar #'parse-element e)))
           (parse-element (element)
             (if (stringp element)
                 element
                 (cond ((eq (car element) :url)
                        (st-json:jso "type" "url"
                                     "addr" (second element)
                                     "description" (or (third element) :null)))
                       ((eq (car element) :code-block)
                        (st-json:jso "type" "code-block"
                                     "language" (or (second element) "text")
                                     "code" (third element)))
                       ((eq (car element) :user)
                        (st-json:jso "type" "user"
                                     "user_id" (second element)
                                     "user_description" (third element)))
                       ((eq (car element) :newline)
                        (st-json:jso "type" "newline"))
                       (t
                        (st-json:jso "type" (ecase (car element)
                                              (:paragraph "p")
                                              (:bold "b")
                                              (:italics "i")
                                              (:code "code")
                                              (:math "math")
                                              (:inline-math "inline-math")
                                              (:quote "quote"))
                                     "e" (parse-element-list (cdr element))))))))
    (let ((parsed (markup-message-content text)))
      (parse-element-list parsed))))
