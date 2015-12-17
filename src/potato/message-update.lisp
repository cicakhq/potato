(in-package :potato.message-update)

(declaim #.potato.common::*compile-decl*)

(define-condition message-update-failed (error)
  ((message :type string
            :initarg :message
            :reader message-update-failed/message))
  (:report (lambda (condition stream)
             (format stream "Message update failed with error: ~a" (message-update-failed/message condition))))
  (:documentation "Error that is raised when an error occurs that is the cause of invalid input parameters"))

(defun raise-message-update-failed (msg &rest msg-args)
  (error 'message-update-failed :message (apply #'format nil msg msg-args)))

(defun handle-message-update-request (req)
  (log:trace "Message update request: ~s" req)
  (destructuring-bind (message-id &key
                                  (image-key nil image-key-p)
                                  (image-size nil image-size-p)
                                  (text nil text-p)
                                  (extra-html nil extra-html-p)
                                  (delete nil delete-p))
      req
    (let ((message (potato.core:load-message message-id)))
      (when (potato.core:message/deleted message)
        (raise-message-update-failed "Attempt to update deleted message: ~s" (potato.core:message/id message)))
      (potato.core:save-message-modification message
                                             (potato.core:message/from message)
                                             (if text-p text (potato.core:message/text message))
                                             (if extra-html-p extra-html (potato.core:message/extra-html message))
                                             (if image-key-p
                                                 (if image-size-p (cons image-key image-size) image-key))
                                             (if delete-p delete (potato.core:message/deleted message))))))

(defun handle-message-post-request (req)
  (destructuring-bind (channel &key text sender extra-html) req
    (let ((channel (potato.db:load-instance 'potato.core:channel channel))
          (sender (if sender
                      (potato.db:load-instance 'potato.core:user sender)
                      nil)))
      (when (and sender
                 (not (potato.core:user-is-in-channel-p channel sender)))
        (raise-message-update-failed "User ~s is not in channel ~s"
                                     (potato.core:user/id sender) (potato.core:channel/id channel)))
      (unless (stringp text)
        (raise-message-update-failed "Text is not a string value"))
      (when (and (equal text "")
                 (null extra-html))
        (raise-message-update-failed "Both text and extra-html are blank"))
      (potato.workflow:send-message-to-channel sender channel text :extra-html extra-html :send-update nil))))

(defun dispatch-message-command (command)
  (handler-case
      (case (car command)
        (:update (handle-message-update-request (cadr command)))
        (:post (handle-message-post-request (cadr command)))
        (t
         (raise-message-update-failed "Unexpected command: ~s" command)))
    (message-update-failed (c)
      (log:error "Error processing message update command: ~s: ~a" command (message-update-failed/message c)))))

(defun message-update-main-loop ()
  (with-rabbitmq-connected (conn)
    (cl-rabbit:basic-consume conn 1 *chat-image-converter-response-queue-name*)
    (loop
       for msg = (cl-rabbit:consume-message conn)
       for decoded-command = (binary-to-lisp (cl-rabbit:message/body (cl-rabbit:envelope/message msg)))
       ;; We'll always ack the message here until we have a good error
       ;; handling policy.
       do (cl-rabbit:basic-ack conn 1 (cl-rabbit:envelope/delivery-tag msg))
       do (dispatch-message-command decoded-command))))

(defun start-message-update-thread ()
  (start-monitored-thread #'message-update-main-loop "Message updater"))
