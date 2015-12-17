(in-package :potato.ws-server)

(declaim #.potato.common::*compile-decl*)

(defvar *server* nil)

(defclass ws-connection (hunchensocket:websocket-resource)
  ((user           :type potato.core:user
                   :initarg :user
                   :reader ws-connection/user)
   (channel        :type potato.core:channel
                   :initarg :channel
                   :reader ws-connection/channel)
   (rchannel       :type (or null cl-rabbit-async:async-channel)
                   :initform nil
                   :accessor ws-connection/rchannel)
   (ws-server-lock :type t
                   :initform (bordeaux-threads:make-lock)
                   :reader ws-connection/lock)
   (mqueue         :type msgl
                   :initform (make-instance 'msgl)
                   :reader ws-connection/mqueue)
   (closed-p       :type t
                   :initform nil
                   :accessor ws-connection/closed-p)
   (queue          :type (or null string)
                   :initarg :queue
                   :initform nil
                   :reader ws-connection/queue)
   (refresh-time   :type (or null number)
                   :initform nil
                   :accessor ws-connection/last-web-session-refresh-time)
   (session        :type t
                   :initarg :session
                   :reader ws-connection/session)))

(defmethod print-object ((conn ws-connection) stream)
  (print-unreadable-safely (user channel queue closed-p) conn stream
    (format stream "USER ~s CHANNEL ~s QUEUE ~s CLOSED-P ~s" user channel queue closed-p)))

(defun verify-and-set-closed (conn)
  (bordeaux-threads:with-lock-held ((ws-connection/lock conn))
    (if (ws-connection/closed-p conn)
        nil
        (progn
          (setf (ws-connection/closed-p conn) t)
          t))))

(defun close-hunchensocket-connection (conn)
  (when (verify-and-set-closed conn)
    (dolist (client (hunchensocket:clients conn))
      (hunchensocket:close-connection client))))

(defmacro with-msgl-task (conn &body body)
  `(msgl-push (ws-connection/mqueue ,conn)
              (lambda ()
                (handler-case
                    (progn ,@body)
                  (error (condition)
                    (log:warn "Error while writing to client: ~a" condition)
                    (msgl-stop-queue))))))

(defun send-hunchensocket-message (conn data)
  (check-type conn ws-connection)
  (let ((clients (hunchensocket:clients conn)))
    (cond ((null clients)
           (log:warn "No recipient of message"))
          ((null (cdr clients))
           (hunchensocket:send-text-message (car clients) (st-json:write-json-to-string data)))
          (t
           (log:error "More than one client: ~s" clients)
           (close-hunchensocket-connection conn)))))

(defmethod hunchensocket:client-connected ((conn ws-connection) user)
  (log:trace "connected: ~s, user: ~s" conn user)
  (let ((consumer-tag nil))
    (labels ((handle-incoming-msg (msg)
               (with-msgl-task conn
                 (let ((potato.core::*current-auth-user* (ws-connection/user conn)))
                   (let ((n (potato.rabbitmq-notifications:process-message
                             msg
                             (bordeaux-threads:with-lock-held ((ws-connection/lock conn))
                               consumer-tag)
                             #'potato.core:notification-message-cd->json-html)))
                     (when n
                       (send-hunchensocket-message conn n))
                     (alexandria:when-let ((rchannel (ws-connection/rchannel conn)))
                       (let ((delivery-tag (cl-rabbit:envelope/delivery-tag msg)))
                         (cl-rabbit-async:async-basic-ack rchannel delivery-tag)))))))
             ;;
             (handle-channel-closed (channel)
               (log:debug "Channel was closed: ~s" channel)
               (setf (ws-connection/rchannel conn) nil)
               (close-hunchensocket-connection conn)))
      ;;
      (let ((potato.core::*current-auth-user* (ws-connection/user conn)))
        (let ((rchannel (cl-rabbit-async:multi-connection-open-channel
                         *rabbitmq-multi-connection-instance*
                         :message-callback #'handle-incoming-msg
                         :close-callback #'handle-channel-closed))
              (http-event (ws-connection/queue conn)))
          (setf (ws-connection/rchannel conn) rchannel)
          (let ((q (if http-event
                       ;; Queue name was specified, check that it is valid
                       (multiple-value-bind (queue-name num-messages num-consumers)
                           (potato.rabbitmq-notifications:declare-notifications-queue-async rchannel http-event t)
                         (declare (ignore queue-name num-messages))
                         (unless (zerop num-consumers)
                           (error "Queue is not idle: ~s" http-event))
                         http-event)
                       ;; ELSE: No existing queue name, create a new one
                       (potato.rabbitmq-notifications:create-and-bind-notifications-queue-async
                        rchannel
                        (ws-connection/user conn)
                        (list (ws-connection/channel conn))
                        potato.rabbitmq-notifications:+all-services+))))
            (bordeaux-threads:with-lock-held ((ws-connection/lock conn))
              (let ((v (cl-rabbit-async:async-basic-consume rchannel q :no-ack nil)))
                (log:trace "Enabled cosume. Consumer tag: ~s" v)
                (setq consumer-tag v)))
            (with-msgl-task conn
              (send-hunchensocket-message conn (st-json:jso "type" "event"
                                                            "event" q)))))))))

(defmethod hunchensocket:client-disconnected ((conn ws-connection) user)
  (log:trace "disconnected: ~s, user: ~s" conn user)
  (let ((rchannel (ws-connection/rchannel conn)))
    (when rchannel
      (log:trace "closing async rabbitmq connection")
      (cl-rabbit-async:close-channel rchannel))
    (potato.core:signout-user (ws-connection/user conn) (ws-connection/channel conn))))

(defmethod hunchensocket:text-message-received ((conn ws-connection) user message)
  (log:trace "got ws message: ~a, conn: ~s" message conn)
  (with-accessors ((refresh ws-connection/last-web-session-refresh-time)) conn
    (let ((data (st-json:read-json-from-string message)))
      (string-case:string-case ((st-json:getjso "cmd" data))
        ("refresh"
         (potato.core:refresh-user (ws-connection/user conn) (ws-connection/channel conn) 60)
         (let ((now (get-universal-time)))
           (when (or (null refresh) (> (+ refresh potato.rabbitmq-notifications::*session-refresh-interval*) now))
             (potato.core::update-persisted-session-data (ws-connection/session conn))
             (setf refresh now)))
         (with-msgl-task conn
           (send-hunchensocket-message conn (st-json:jso "type" "refresh"
                                                         "data" (st-json:getjso "data" data)))))
        (t
         (log:error "Unexpected message from client: ~s" message))))))

(defun make-client-instance (request)
  (log:trace "websocket connection initialisation for: ~s" (hunchentoot:script-name request))
  (multiple-value-bind (match strings)
      (cl-ppcre:scan-to-strings "^/ws/([^/]+)$" (hunchentoot:script-name request))
    (unless match
      (potato.core:raise-not-found-error "Illegal websocket resource"))
    (let ((cid (aref strings 0))
          (http-event (hunchentoot:parameter "event")))
      (potato.core:with-authenticated-user ()
        (let ((channel (potato.core:load-channel-with-check cid)))
          (let ((conn (make-instance 'ws-connection
                                     :channel channel
                                     :user (potato.core:current-user)
                                     :queue http-event
                                     :session (potato.core:find-current-user-session))))
            (log:trace "returning connection: ~s" conn)
            conn))))))

(defmethod hunchentoot:acceptor-dispatch-request :around ((acceptor hunchensocket:websocket-acceptor) request)
  (handler-case
      (call-next-method)
    (potato.core:permission-error (condition)
      (log:trace "Permission error when creating ws connection")
      (setf (hunchentoot:return-code*) (potato.core:potato-error/response-status condition))
      nil)))

(potato.common.application:define-component ws-server
  (:dependencies potato.common::generic)
  (:start
   (let ((server (make-instance 'hunchensocket:websocket-acceptor :port *websocket-listen-port*)))
     (pushnew 'make-client-instance hunchensocket:*websocket-dispatch-table* )
     (setq *server* server)
     (hunchentoot:start server)
     (log:info "Websocket listener started"))))
