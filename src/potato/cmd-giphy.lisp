(defpackage :potato.slashcommand.giphy
  (:use :cl :potato :potato.common))

(in-package :potato.slashcommand.giphy)

(declaim #.potato.common::*compile-decl*)

(defun make-html-from-template (url)
  (babel:octets-to-string
   (flexi-streams:with-output-to-sequence (s)
     (lofn:show-template s "giphy-data.tmpl" `((:url . ,url))))))

(defun parse-result (uid cid data)
  (let ((results (st-json:getjso "data" data)))
    (when results
      (alexandria:when-let ((url (st-json:getjso* "images.fixed_height.url" (first results))))
        (log:info "Will send message with url: ~s" url)
        (let ((user (potato.db:load-instance 'potato.core:user uid))
              (channel (potato.db:load-instance 'potato.core:channel cid)))
          (unless (potato.core:user-is-in-channel-p channel user)
            (error "Attempt to send a gif to a channel that the user is not in"))
          (potato.workflow:send-message-to-channel user channel "Animated image from giphy"
                                                   :extra-html (make-html-from-template url)))))))

(defun process-gif-command (uid cid args)
  (let ((trimmed (potato.core:trim-string args)))
    (when (plusp (length trimmed))
      (multiple-value-bind (body code headers orig-url stream should-close reason)
          (drakma:http-request "http://api.giphy.com/v1/gifs/search"
                               :parameters `(("q" . ,trimmed)
                                             ("api_key" . "dc6zaTOxFJmzC"))
                               :want-stream t)
        (declare (ignore body headers orig-url))
        (unwind-protect
             (if (= code hunchentoot:+http-ok+)
                 (let ((data (st-json:read-json stream)))
                   (parse-result uid cid data))
                 ;; ELSE: Request failed
                 (log:error "Giphy request failed. code=~s, reason=~s" code reason))
          (when should-close
            (close stream)))))))

(defun process-command-loop ()
  (potato.slashcommand:command-processor-loop (args uid cid)
   ("gif" (process-gif-command uid cid args))))

(potato.common.application:define-component slashcommand-giphy
  (:dependencies potato.common::rabbitmq)
  (:start
   (start-monitored-thread #'process-command-loop "Giphy command processor")))
