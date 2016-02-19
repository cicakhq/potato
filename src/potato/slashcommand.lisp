(in-package :potato.slashcommand)

(declaim #.potato.common::*compile-decl*)

(defun validate-command-name (cmd)
  (unless (stringp cmd)
    (potato.core:raise-web-parameter-error "Command must be a string: ~s" cmd))
  (unless (cl-ppcre:scan "^[a-zA-Z0-9]+$" cmd)
    (potato.core:raise-web-parameter-error "Illegal command format: ~s" cmd))
  (string-downcase cmd))

(defun send-slashcommand (channel user cmd args)
  (check-type cmd string)
  (check-type args string)
  (let ((channel (potato.core:ensure-channel channel)))
    (with-pooled-rabbitmq-connection (conn)
      (cl-rabbit:basic-publish conn 1
                               :exchange *slashcommand-request-exchange-name*
                               :routing-key (format nil "~a.~a.~a.~a"
                                                    (potato.core:channel/domain channel)
                                                    (potato.core:channel/id channel)
                                                    (encode-name-for-routing-key (potato.core:ensure-user-id user))
                                                    cmd)
                               :body (lisp-to-binary (list cmd args))
                               :properties `((:headers . (("channel" . ,(potato.core:channel/id channel))
                                                          ("domain" . ,(potato.core:channel/domain channel))
                                                          ("user" . ,(potato.core:ensure-user-id user))
                                                          ("cmd" . ,cmd))))))))

(potato.core:define-json-handler-fn-login (slashcommand-screen "/command" data nil ())
  (potato.core:with-authenticated-user ()
    (let ((channel (potato.core:load-channel-with-check (st-json:getjso "channel" data)))
          (command (validate-command-name (st-json:getjso "command" data)))
          (args (st-json:getjso "arg" data)))
      (unless (stringp args)
        (potato.core:raise-web-parameter-error "arg parameter must be a string"))
      (send-slashcommand channel (potato.core:current-user) command args)
      (st-json:jso "result" "ok"))))

(defun poll-for-commands (commands callback-fn)
  (with-rabbitmq-connected (conn)
    (let ((queue (cl-rabbit:queue-declare conn 1 :auto-delete t)))
      (dolist (command commands)
        (cl-rabbit:queue-bind conn 1 :queue queue
                                     :exchange *slashcommand-request-exchange-name*
                                     :routing-key (format nil "*.*.*.~a" command)))
      (cl-rabbit:basic-consume conn 1 queue :no-ack t)
      (loop
        for msg = (cl-rabbit:consume-message conn)
        do (funcall callback-fn msg)))))

(defmacro command-processor-loop ((args-sym &optional uid-sym channel-sym domain-sym) &body all-defs)
  (check-type args-sym symbol)
  (check-type uid-sym (or null symbol))
  (check-type channel-sym (or null symbol))
  (check-type domain-sym (or null symbol))
  (let ((msg-sym (gensym "MESSAGE-"))
        (cmd-sym (gensym "CMD-"))
        (args-int-sym (gensym "ARGS-"))
        (body-sym (gensym "BODY-"))
        (headers-sym (gensym "HEADERS-")))
    `(poll-for-commands
      ',(mapcar #'car all-defs)
      (lambda (,msg-sym)
        (let ((,body-sym (cl-rabbit:envelope/message ,msg-sym)))
          (destructuring-bind (,cmd-sym ,args-int-sym)
              (binary-to-lisp (cl-rabbit:message/body ,body-sym))
            (let ((,headers-sym (cdr (assoc :headers (cl-rabbit:message/properties ,body-sym)))))
              (declare (ignorable ,headers-sym))
              (string-case:string-case (,cmd-sym)
                ,@(loop
                    for definition in all-defs
                    collect (destructuring-bind (cmd-def &rest body-def)
                                definition
                              (check-type cmd-def string)
                              (check-type body-def list)
                              `(,cmd-def (let ((,args-sym ,args-int-sym)
                                               ,@(if uid-sym
                                                     (list `(,uid-sym (cdr (assoc "user" ,headers-sym :test #'equal)))))
                                               ,@(if channel-sym
                                                     (list `(,channel-sym (cdr (assoc "channel" ,headers-sym :test #'equal)))))
                                               ,@(if domain-sym
                                                     (list `(,domain-sym (cdr (assoc "domain" ,headers-sym :test #'equal))))))
                                           ,@body-def))))))))))))

;;;
;;;  Default slashcommand processor
;;;

(defun process-foo-command (args uid cid domain-id)
  (log:info "Foo command from user=~s, channel=~d, domain=~s: ~s" uid cid domain-id args))

(defun slashcommand-default-loop ()
  (command-processor-loop (args uid cid domain-id)
    ("foo" (process-foo-command args uid cid domain-id))))

(potato.common.application:define-component slashcommand-default
  (:dependencies potato.common::rabbitmq)
  (:start
   (start-monitored-thread #'slashcommand-default-loop "Slashcommand default")))
