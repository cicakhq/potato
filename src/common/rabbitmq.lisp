(in-package :potato.common)

(declaim #.*compile-decl*)

(defvar *rabbitmq-server-name* "localhost")
(defvar *rabbitmq-server-port* 5672)
(defvar *rabbitmq-user* "guest")
(defvar *rabbitmq-password* "guest")
(defvar *rabbitmq-vhost* "/")

(defvar *rabbitmq-multi-connection-instance* nil)

(defparameter *unread-state-exchange-name* "unread-state-ex")
(defparameter *channel-content-exchange-name* "channel-content-ex")
(defparameter *channel-exchange-name* "channel-ex")
(defparameter *user-notifications-exchange-name* "user-notifications-ex")
(defparameter *gcm-queue-name* "user-notifications-gcm-send")

(defparameter *message-send-exchange-name* "message-send-ex"
  "Exchange that reecieves all messages that are sent in the system.
This is different from channel-content-ex in that this exchange only
includes normal user messages, and not modifications, file uploads and
system messages. The routing key has the following format:
DOMAIN.CHANNEL.SENDER")

(defparameter *content-processor-queue-name* "content-processor-queue")

(defparameter *state-server-reader-exchange-name* "state-server-reader-ex")
(defparameter *state-server-sender-exchange-name* "state-server-sender-ex")

(defparameter *chat-image-converter-acceptor-exchange-name* "chat-image-accept-ex")
(defparameter *chat-image-converter-acceptor-queue-name* "chat-image-accept")
(defparameter *chat-image-converter-response-exchange-name* "chat-image-response-ex")
(defparameter *chat-image-converter-response-queue-name* "chat-image-queue")

(defparameter *email-exchange-name* "email-ex")
(defparameter *email-queue-name* "email-queue")

(defparameter *state-server-message-channel-join* "channel"
  "RabbitMQ message to the state server when a user enters a channel")
(defparameter *state-server-message-channel-join-with-timeout* "chtime"
  "RabbitMQ message to the state server when a user enters a channel, with explicit timeout")
(defparameter *state-server-message-typing-start* "type-begin"
  "RabbitMQ message to the state server when a user starts typing")
(defparameter *state-server-message-typing-end* "type-end"
  "RabbitMQ message to the state server when a user stops typing")
(defparameter *state-server-message-sync-users* "sync"
  "RabbitMQ message to the state server to request a full channel sync")

(defparameter *state-server-reply-user-add-to-channel* "add"
  "RabbitMQ message from the state server when a user enters a channel")
(defparameter *state-server-reply-user-remove-from-channel* "remove"
  "RabbitMQ message from the state server when a user leaves a channel")
(defparameter *state-server-reply-typing-start* "type-begin"
  "RabbitMQ message from the state server when a user starts typing")
(defparameter *state-server-reply-typing-end* "type-end"
  "RabbitMQ message from the state server when a user stops typing")
(defparameter *state-server-reply-sync-users* "sync-users"
  "RabbitMQ message from the state server when sending a full user sync")

(defparameter *state-server-reader-low-prio* 1
  "The priority level for low priority messages to the state server.")
(defparameter *state-server-reader-high-prio* 1
  "The priority level for high priority messages to the state server.")
(defparameter *state-server-reader-expire-time* nil ;"10000"
  "The default expiry time for low-priority messages to the state
server. Note that the value of this variable is a string. This is
because that is the type RabbitMQ API expects for the expiry value.")

(defparameter *slashcommand-request-exchange-name* "slashcommand-ex"
  "Exchange to which slash commands are sent. The routing key has the
following form: DOMAIN.CHANNEL.USER.COMMAND")
(defparameter *slashcommand-unrouted-command-exchange* "slashcommand-unrouted-ex"
  "Exchange that is bound to unrouted commands from *SLASHCOMMAND-REQUEST-EXCHANGE-NAME*")
(defparameter *slashcommand-unrouted-command-queue* "slashcommand-unrouted"
  "Queue that is bound to unrouted commands from *SLASHCOMMAND-REQUEST-EXCHANGE-NAME*")

(defun connect-to-rabbitmq ()
  (handler-bind ((error (lambda (condition)
                          (log:error "Error when connection to rabbitmq: ~a" condition))))
    (let* ((conn (cl-rabbit:new-connection))
           (socket (cl-rabbit:tcp-socket-new conn)))
      (cl-rabbit:socket-open socket *rabbitmq-server-name* *rabbitmq-server-port*)
      (cl-rabbit:login-sasl-plain conn *rabbitmq-vhost* *rabbitmq-user* *rabbitmq-password*)
      conn)))

(defun destroy-rabbitmq-connection (conn)
  (cl-rabbit:destroy-connection conn))

(defun make-rabbitmq-connection-pool (&key (capacity 5))
  (pooler:make-pool :name "RabbitMQ connection pool"
                    :capacity capacity
                    :item-maker #'connect-to-rabbitmq
                    :item-destroyer #'destroy-rabbitmq-connection))

(defvar *rabbitmq-pool* (make-rabbitmq-connection-pool))

(defmacro with-pooled-rabbitmq-connection ((conn) &body body)
  (let ((conn-sym (gensym "CONN-"))
        (channel-closed-sym (gensym "CLOSED-"))
        (failed-p-sym (gensym "FAILED-P-")))
    `(let ((,conn-sym (pooler:fetch-from *rabbitmq-pool* :tries 10))
           (,channel-closed-sym nil)
           (,failed-p-sym nil))
       (unwind-protect
            (handler-bind ((error (lambda (condition)
                                    (declare (ignore condition))
                                    (setq ,failed-p-sym t))))
              (cl-rabbit:channel-open ,conn-sym 1)
              (let ((,conn ,conn-sym))
                ,@body)
              (cl-rabbit:channel-close ,conn-sym 1)
              (setq ,channel-closed-sym t))
         (if (and ,channel-closed-sym (not ,failed-p-sym))
             ;; Connection is still working, return it to the pool
             (pooler:return-to *rabbitmq-pool* ,conn-sym)
             ;; ELSE: The connection isn't valid anymore. Attempt to close it.
             (handler-case
                 (cl-rabbit:destroy-connection ,conn-sym)
               (error (condition) (log:error "Unable to close connection after error: ~a" condition))))))))

(defmacro with-rabbitmq-connected ((conn &optional socket) &body body)
  (let ((conn-sym (gensym "CONN-"))
        (socket-sym (gensym "SOCKET-")))
    `(cl-rabbit:with-connection (,conn-sym)
       (let ((,socket-sym (cl-rabbit:tcp-socket-new ,conn-sym)))
         (cl-rabbit:socket-open ,socket-sym *rabbitmq-server-name* *rabbitmq-server-port*)
         (cl-rabbit:login-sasl-plain ,conn-sym *rabbitmq-vhost* *rabbitmq-user* *rabbitmq-password*)
         (cl-rabbit:channel-open ,conn-sym 1)
         (let ((,conn ,conn-sym)
               ,@(if socket `((,socket ,socket-sym))))
           ,@body)))))

(defmacro rabbitmq-queue-processor ((name message binding &key bind-queue-name no-ack) &body body)
  (check-type name symbol)
  (check-type message symbol)
  (let ((conn-sym (gensym "CONN-"))
        (queue-sym (gensym "QUEUE-"))
        (message-sym (gensym "MESSAGE-")))
    `(with-rabbitmq-connected (,conn-sym)
       (let ((,queue-sym ,(ecase (car binding)
                            (:exchange
                             (let ((qs (gensym)))
                               `(let ((,qs (cl-rabbit:queue-declare ,conn-sym 1 :auto-delete 1 :durable nil)))
                                  (cl-rabbit:queue-bind ,conn-sym 1 :queue ,qs :exchange ,(cadr binding) :routing-key "#")
                                  ,qs)))
                            (:queue
                             (cadr binding)))))
         (cl-rabbit:basic-consume ,conn-sym 1 ,queue-sym :no-ack ,no-ack)
         (let (,@(if bind-queue-name
                     `((,bind-queue-name ,queue-sym))))
           (loop
              for ,message-sym = (cl-rabbit:consume-message ,conn-sym)
              do (let ((,message ,message-sym))
                   ,@body)))))))

(potato.common.application:define-component rabbitmq
  (:start
   (with-rabbitmq-connected (conn)
     (cl-rabbit:exchange-declare conn 1 *unread-state-exchange-name* "topic" :durable t)
     (cl-rabbit:exchange-declare conn 1 *channel-content-exchange-name* "topic" :durable t)
     (cl-rabbit:exchange-declare conn 1 *channel-exchange-name* "topic" :durable t)

     (cl-rabbit:exchange-declare conn 1 *user-notifications-exchange-name* "topic" :durable t)
     ;; GCM message processor queue
     (cl-rabbit:queue-declare conn 1 :queue *gcm-queue-name* :durable t :arguments `(("x-message-ttl" . ,(* 60 60 1000))))
     (cl-rabbit:queue-bind conn 1
                           :queue *gcm-queue-name*
                           :exchange *user-notifications-exchange-name*
                           :routing-key "#")

     (cl-rabbit:exchange-declare conn 1 *state-server-reader-exchange-name* "topic" :durable t)
     (cl-rabbit:exchange-declare conn 1 *state-server-sender-exchange-name* "topic" :durable t)

     (cl-rabbit:exchange-declare conn 1 *chat-image-converter-acceptor-exchange-name* "topic" :durable t)
     (cl-rabbit:queue-declare conn 1 :queue *chat-image-converter-acceptor-queue-name* :durable t)
     (cl-rabbit:queue-bind conn 1
                           :queue *chat-image-converter-acceptor-queue-name*
                           :exchange *chat-image-converter-acceptor-exchange-name*
                           :routing-key "#")

     (cl-rabbit:exchange-declare conn 1 *chat-image-converter-response-exchange-name* "topic" :durable t)
     (cl-rabbit:queue-declare conn 1 :queue *chat-image-converter-response-queue-name* :durable t)
     (cl-rabbit:queue-bind conn 1
                           :queue *chat-image-converter-response-queue-name*
                           :exchange *chat-image-converter-response-exchange-name*
                           :routing-key "#")

     (cl-rabbit:exchange-declare conn 1 *email-exchange-name* "topic" :durable t)
     (cl-rabbit:queue-declare conn 1 :queue *email-queue-name* :durable t)
     (cl-rabbit:queue-bind conn 1
                           :queue *email-queue-name*
                           :exchange *email-exchange-name*
                           :routing-key "#")

     (cl-rabbit:exchange-declare conn 1 *message-send-exchange-name* "topic" :durable t)

     ;; Messages on the content processor queue will only live for 10
     ;; seconds. If a message has not been processed before then,
     ;; it's better to drop it than to have it being processed at
     ;; some random point in the future.
     (cl-rabbit:queue-declare conn 1 :queue *content-processor-queue-name* :arguments '(("x-message-ttl" . 10000)))
     (cl-rabbit:queue-bind conn 1
                           :queue *content-processor-queue-name*
                           :exchange *message-send-exchange-name*
                           :routing-key "#")

     ;; Exchanges and queues that handle slash commands
     (cl-rabbit:exchange-declare conn 1 *slashcommand-unrouted-command-exchange* "topic" :durable t)
     (cl-rabbit:exchange-declare conn 1 *slashcommand-request-exchange-name* "topic"
                                 :durable t
                                 :arguments `(("alternate-exchange" . ,*slashcommand-unrouted-command-exchange*)))
     (cl-rabbit:queue-declare conn 1 :queue *slashcommand-unrouted-command-queue* :durable t)
     (cl-rabbit:queue-bind conn 1
                           :queue *slashcommand-unrouted-command-queue*
                           :exchange *slashcommand-unrouted-command-exchange*
                           :routing-key "#")

     ;; Initialise support for multi-connection
     (setq *rabbitmq-multi-connection-instance*
           (cl-rabbit-async:make-multi-connection *rabbitmq-server-name*
                                                  :port *rabbitmq-server-port*
                                                  :user *rabbitmq-user*
                                                  :password *rabbitmq-password*
                                                  :vhost *rabbitmq-vhost*)))))

(defun send-rabbitmq-message-to-state-server (cid high-prio-p message &key connection)
  (labels ((send-message (conn)
             (let ((props (if high-prio-p
                              `((:priority . ,*state-server-reader-high-prio*))
                              `((:priority . ,*state-server-reader-low-prio*)
                                ,@(if *state-server-reader-expire-time*
                                      `((:expiration . ,*state-server-reader-expire-time*))
                                      nil)))))
               (cl-rabbit:basic-publish conn 1
                                        :exchange *state-server-reader-exchange-name*
                                        :routing-key cid
                                        :body (lisp-to-binary message)
                                        :properties props))))
    (if connection
        (send-message connection)
        (with-pooled-rabbitmq-connection (conn)
          (send-message conn)))))
