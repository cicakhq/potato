(defpackage :potato.slashcommand.giphy
  (:use :cl :potato :potato.common))

(in-package :potato.slashcommand.giphy)

(declaim #.potato.common::*compile-decl*)

(defparameter *max-num-results* 5)

(defun make-html-from-template (url)
  (babel:octets-to-string
   (flexi-streams:with-output-to-sequence (s)
     (lofn:show-template s "giphy-data.tmpl" `((:url . ,url))))))

(defun parse-result (uid sid cid data)
  (let ((results (st-json:getjso "data" data)))
    (when results
      (let ((user (potato.db:load-instance 'potato.core:user uid))
            (channel (potato.db:load-instance 'potato.core:channel cid)))
        (unless (potato.core:user-is-in-channel-p channel user)
          (error "Attempt to send a gif to a channel that the user is not in"))
        (with-pooled-rabbitmq-connection (conn)
          (cl-rabbit:basic-publish conn 1
                                   :exchange *session-notifications-exchange-name*
                                   :routing-key (format nil "~a.~a.~a"
                                                        (encode-name-for-routing-key uid)
                                                        (encode-name-for-routing-key sid)
                                                        (encode-name-for-routing-key cid))
                                   :body (lisp-to-binary
                                          (list potato.rabbitmq-notifications:*session-notification-option*
                                                "xx" cid "Choose a picture"
                                                (loop
                                                  for e in results
                                                  for i from 1 to *max-num-results*
                                                  collect (let ((url (st-json:getjso* "images.fixed_height.url" e)))
                                                            (list (format nil "~a" i)
                                                                  url
                                                                  :image-url url)))))))))))

(defun process-gif-command (args uid sid cid)
  (let ((trimmed (potato.core:trim-string args)))
    (when (plusp (length trimmed))
      (multiple-value-bind (body code headers orig-url stream should-close reason)
          (drakma:http-request "http://api.giphy.com/v1/gifs/search"
                               :parameters `(("q" . ,trimmed)
                                             ("api_key" . "dc6zaTOxFJmzC")
                                             ("limit" . ,(princ-to-string *max-num-results*)))
                               :want-stream t)
        (declare (ignore body headers orig-url))
        (unwind-protect
             (if (= code hunchentoot:+http-ok+)
                 (let ((data (st-json:read-json stream)))
                   (parse-result uid sid cid data))
                 ;; ELSE: Request failed
                 (log:error "Giphy request failed. code=~s, reason=~s" code reason))
          (when should-close
            (close stream)))))))

(defun process-command-loop ()
  (potato.slashcommand:command-processor-loop (args uid sid cid)
   ("gif" (process-gif-command args uid sid cid))))

(potato.common.application:define-component slashcommand-giphy
  (:dependencies potato.common::rabbitmq)
  (:start
   (start-monitored-thread #'process-command-loop "Giphy command processor")))
