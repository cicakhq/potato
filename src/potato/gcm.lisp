(defpackage :potato.gcm
  (:use :cl :potato :potato.common)
  (:export #:start-gcm-listener
           #:*gcm-authorisation-key*))

(in-package :potato.gcm)

(declaim #.potato.common::*compile-decl*)

(defvar *gcm-authorisation-key* nil)

(defun push-gcm-message (gcm-key message-id)
  (let ((content (st-json:jso "to" gcm-key
                              "message_id" message-id
                              "data" (st-json:jso "field1" "abc" "field2" "otherdata"))))
    (drakma:http-request "https://gcm-http.googleapis.com/gcm/send"
                         :method :post
                         :content-type "application/json"
                         :content (st-json:write-json-to-string content)
                         :additional-headers `((:authorization . "key=AIzaSyBVxTvHCGVd-a0HVHzKYKb-eWsE7MeSRr4")))))

(defun process-gcm-user-notification (msg)
  (let ((message (cl-rabbit:envelope/message msg)))
    (destructuring-bind (user-id created-date message-id)
        (read-from-string (babel:octets-to-string (cl-rabbit:message/body message) :encoding :utf-8))
      (declare (ignore created-date))
      (let ((user (potato.db:load-instance 'potato.core:user user-id)))
        (alexandria:when-let ((gcm-key (potato.core:user/android-gcm-key user)))
          (push-gcm-message gcm-key message-id))))))

(defun gcm-listener-loop ()
  (with-rabbitmq-connected (conn)
    (cl-rabbit:basic-consume conn 1 *gcm-queue-name* :no-ack t)
    (loop
       for msg = (cl-rabbit:consume-message conn)
       do (process-gcm-user-notification msg))))

(defun start-gcm-listener ()
  (start-monitored-thread #'gcm-listener-loop "GCM listener loop"))
