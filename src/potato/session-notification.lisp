(in-package :potato.rabbitmq-notifications)

(declaim #.potato.common::*compile-decl*)

(defparameter *session-notification-unknown-slashcommand* "unknown-slashcommand")
(defparameter *session-notification-option* "option")

(defun make-options-data (data)
  (destructuring-bind (option-id option-title options)
      data
    (st-json:jso "option-code" option-id
                 "title" option-title
                 "options" (mapcar (lambda (v)
                                     (destructuring-bind (title response)
                                         v
                                       (st-json:jso "title" title
                                                    "response" response)))
                                   options))))

(defun process-session-notification (msg)
  (let* ((message (cl-rabbit:envelope/message msg))
         (body (binary-to-lisp (cl-rabbit:message/body message)))
         (cmd (car body)))
    (string-case:string-case (cmd)
      (#.*session-notification-unknown-slashcommand* (st-json:jso "unknown-slashcommand" (second body)))
      (#.*session-notification-option* (st-json:jso "option" (make-options-data (cdr body))))
      (t (log:warn "Unexpected session command: ~s" cmd)))))
