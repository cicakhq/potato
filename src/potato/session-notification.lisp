(in-package :potato.rabbitmq-notifications)

(declaim #.potato.common::*compile-decl*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *session-notification-unknown-slashcommand* "unknown-slashcommand")
  (defparameter *session-notification-option* "option"))

(defun make-options-data (data)
  (destructuring-bind (option-id cid option-title options)
      data
    (st-json:jso "type" "option"
                 "channel" cid
                 "option-code" option-id
                 "title" option-title
                 "options" (mapcar (lambda (v)
                                     (destructuring-bind (title response)
                                         v
                                       (st-json:jso "title" title
                                                    "response" response)))
                                   options))))

(defun make-unknown-slashcomamnd-data (data)
  (st-json:jso "type" "unknown-slashcommand"
               "cmd" (first data)
               "channel" (second data)))

(defun process-session-notification (msg)
  (let* ((message (cl-rabbit:envelope/message msg))
         (body (binary-to-lisp (cl-rabbit:message/body message)))
         (cmd (car body)))
    (string-case:string-case (cmd)
      (#.*session-notification-unknown-slashcommand* (make-unknown-slashcomamnd-data (cdr body)))
      (#.*session-notification-option* (make-options-data (cdr body)))
      (t (log:warn "Unexpected session command: ~s" cmd)))))
