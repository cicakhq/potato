(in-package :potato.rabbitmq-notifications)

(declaim #.potato.common::*compile-decl*)

(alexandria:define-constant +crlf+ (format nil "~c~c" #\Return #\Newline) :test 'equal)
(alexandria:define-constant +nl+ (format nil "~c" #\Newline) :test 'equal)
(alexandria:define-constant +all-services+
    '(:content-p t :user-state-p t :user-notifications-p t :unread-p t :channel-updates-p t :session-p t)
  :test 'equal)

(defvar *notifications-timer-queue* (potato.common.timer:make-timer-queue :name "RabbitMQ Notifications timer queue"))

(defparameter *ping-interval* 30)
(defparameter *session-refresh-interval* (* 15 60))

(defstruct active-eventsource-socket-data request socket)
(defvar *active-eventsource-sockets* (dhs-sequences:make-cas-wrapper (fset:empty-set)))

(defun cleanup-name (string)
  (with-output-to-string (s)
    (loop
      for ch across string
      for v = (char-code ch)
      if (or (<= (char-code #\a) v (char-code #\z))
             (<= (char-code #\A) v (char-code #\Z))
             (<= (char-code #\0) v (char-code #\9))
             (find ch "-_."))
        do (write-char ch s)
      else do (format s "+~2,'0x" v))))

(defmacro define-async-sync-function (sync-name async-name (&rest functions) (&rest args) &body body)
  (let ((conn-sym (gensym "CONN-"))
        (chnum-sym (gensym "CHANNEL-INDEX-"))
        (rchannel-sym (gensym "CHANNEL-")))
    (multiple-value-bind (rem-forms declarations doc-string)
        (alexandria:parse-body body :documentation t)
      `(progn
         (defun ,sync-name (,conn-sym ,chnum-sym ,@args)
           ,@(if doc-string (list doc-string))
           ,@declarations
           (macrolet ,(loop
                        for (fn sync-fn async-fn) in functions
                        for arg = (gensym)
                        collect `(,fn (&rest ,arg) (append '(,sync-fn ,conn-sym ,chnum-sym) ,arg)))
             ,@rem-forms))
         (defun ,async-name (,rchannel-sym ,@args)
           ,@(if doc-string (list doc-string))
           ,@declarations
           (macrolet ,(loop
                        for (fn sync-fn async-fn) in functions
                        for arg = (gensym)
                        collect `(,fn (&rest ,arg) (append '(,async-fn ,rchannel-sym) ,arg)))
             ,@rem-forms))))))

(define-async-sync-function declare-notifications-queue declare-notifications-queue-async
    ((d cl-rabbit:queue-declare cl-rabbit-async:async-queue-declare))
    (name passive)
  (d :queue name :durable nil :exclusive nil :auto-delete nil :passive passive
     :arguments '(("x-expires" . 30000))))

(define-async-sync-function request-full-state-server-sync request-full-state-server-sync-async
    ((p cl-rabbit:basic-publish cl-rabbit-async:async-basic-publish))
    (cid queue)
  (p :exchange *state-server-reader-exchange-name*
     :routing-key cid
     :body (lisp-to-binary (list *state-server-message-sync-users*
                                 cid
                                 (encode-name-for-routing-key queue)))
     :properties `((:priority . ,*state-server-reader-high-prio*))))

(defun print-queue-name-prefix (stream uid cid-list)
  (princ "q-" stream)
  (princ (cleanup-name uid) stream)
  (princ "-" stream)
  (when cid-list
    (let ((d (ironclad:make-digest :sha256)))
      (dolist (cid cid-list)
        (ironclad:update-digest d (babel:string-to-octets cid :encoding :utf-8)))
      (princ (ironclad:byte-array-to-hex-string (subseq (ironclad:produce-digest d) 0 16)) stream))))

(defun verify-queue-name (name cid-list)
  (when name
    (let ((prefix (with-output-to-string (s)
                    (print-queue-name-prefix s (potato.core:user/id (potato.core:current-user)) cid-list))))
      (unless (string-start-match prefix name)
        (error "Attempt to read from illegal queue")))))

(defun make-queue-name (uid cid-list)
  (when (null cid-list)
    (error "Can't make a queue name without a channel list"))
  (with-output-to-string (s)
    (print-queue-name-prefix s uid cid-list)
    (princ "-" s)
    (princ (make-random-name 20) s)))

(define-async-sync-function add-new-channel-binding add-new-channel-binding-async
    ((d declare-notifications-queue declare-notifications-queue-async)
     (r request-full-state-server-sync request-full-state-server-sync-async)
     (b cl-rabbit:queue-bind cl-rabbit-async:async-queue-bind))
    (queue-name cid &key content-p user-state-p channel-updates-p)
  "Add a new channel binding for channel CID on queue QUEUE-NAME."
  (unless (or content-p user-state-p channel-updates-p)
    (error "At least one of :CONTENT-P :USER-STATE-P :CHANNEL-UPDATES-P must be specified"))
  (when content-p
    (b :queue queue-name
       :exchange *channel-content-exchange-name*
       :routing-key cid))
  (when user-state-p
    (b :queue queue-name
       :exchange *state-server-sender-exchange-name*
       :routing-key (format nil "change.*.~a.*.all" cid))
    (b :queue queue-name
       :exchange *state-server-sender-exchange-name*
       :routing-key (format nil "sync.*.~a.*.~a"
                            (encode-name-for-routing-key cid)
                            (encode-name-for-routing-key queue-name))))
  (when channel-updates-p
    (b :queue queue-name
       :exchange *channel-exchange-name*
       :routing-key (format nil "*.~a" (encode-name-for-routing-key cid))))
  (when user-state-p
    (r cid queue-name)))

(define-async-sync-function create-and-bind-notifications-queue create-and-bind-notifications-queue-async
    ((d declare-notifications-queue declare-notifications-queue-async)
     (b cl-rabbit:queue-bind cl-rabbit-async:async-queue-bind)
     (a add-new-channel-binding add-new-channel-binding-async))
    (user channels services sid)
  (destructuring-bind (&key
                         content-p user-state-p user-notifications-p
                         unread-p channel-updates-p session-p)
      services
    (when (and (not content-p)
               (not user-state-p)
               (not user-notifications-p)
               (not unread-p)
               (not channel-updates-p)
               (not session-p))
      (error "At least one service must be enabled"))
    (let* ((uid (potato.core:ensure-user-id user))
           (encoded-uid (encode-name-for-routing-key uid))
           (cid-list (mapcar #'potato.core:ensure-channel-id channels)))
      (let ((queue-name (make-queue-name uid cid-list)))
        (d queue-name nil)
        (dolist (cid cid-list)
          (a queue-name cid :content-p content-p :user-state-p user-state-p :channel-updates-p channel-updates-p))
        (when user-notifications-p
          (b :queue queue-name
             :exchange *user-notifications-exchange-name*
             :routing-key (format nil "*.~a" encoded-uid)))
        (when unread-p
          (b :queue queue-name
             :exchange *unread-state-exchange-name*
             :routing-key (format nil "~a.*" encoded-uid)))
        (when (and session-p sid)
          (b :queue queue-name
             :exchange *session-notifications-exchange-name*
             :routing-key (format nil "~a.~a.*" encoded-uid (encode-name-for-routing-key sid))))
        queue-name))))

(defun process-channel-message (msg msg-formatter)
  (let* ((body (cl-rabbit:message/body (cl-rabbit:envelope/message msg)))
         (content (potato.common:decode-conspack-with-interning body)))
    (st-json:jso "type" "m"
                 "c" (funcall msg-formatter content))))

(defun process-state-server-message (msg)
  (flet ((make-add-remove-message (args add-p)
           (destructuring-bind (cid uid) args
             (st-json:jso "type" "cu"
                          "user" uid
                          "channel" cid
                          "add-type" (if add-p "add" "remove"))))
         (make-type-message (args begin-p)
           (destructuring-bind (cid uid) args
             (if (string= uid (potato.core:user/id (potato.core:current-user)))
                 ;; The typing message is for the user itself so it can be ignored
                 nil
                 ;; ELSE: Message is for another user, send it to the client
                 (st-json:jso "type" "type"
                              "user" uid
                              "channel" cid
                              "add-type" (if begin-p "begin" "end")))))

         (make-sync-message (args)
           ;; Message format from state server: (CHANNEL ((UID NAME) ...))
           ;; Create a list of the following form:
           ;;   {type: "sync", element: {users: [{id: ID, description: NAME}, ...]}}
           (let ((users (mapcar (lambda (v) (st-json:jso "id" (car v))) (second args))))
             (st-json:jso "type" "cu"
                          "channel" (first args)
                          "users" users
                          "add-type" "sync"))))

    (let* ((body (cl-rabbit:message/body (cl-rabbit:envelope/message msg)))
           (content (binary-to-lisp body))
           (key (car content))
           (args (cdr content)))
      (string-case:string-case (key)
        ("add"         (make-add-remove-message args t))
        ("remove"      (make-add-remove-message args nil))
        ("type-begin"  (make-type-message args t))
        ("type-end"    (make-type-message args nil))
        ("sync-users"  (make-sync-message args))
        (t (error "No match for string: ~s" key))))))

(defun process-user-notification-message (msg)
  (let* ((body (cl-rabbit:message/body (cl-rabbit:envelope/message msg)))
         (content (binary-to-lisp body)))
    (potato.user-notification:process-user-notification-message-and-build-json content)))

(defun process-unread-message (msg)
  (let* ((body (cl-rabbit:message/body (cl-rabbit:envelope/message msg)))
         (content (potato.common:decode-conspack-with-interning body)))
    (st-json:jso "type" "unread"
                 "channel" (potato.core:user-unread-state-rabbitmq-message/channel content)
                 "count" (potato.core:user-unread-state-rabbitmq-message/count content))))

(defun process-message (msg subscription-consumer-tag msg-formatter)
  (let ((consumer (cl-rabbit:envelope/consumer-tag msg)))
    (unless (equal consumer subscription-consumer-tag)
      ;; If this error occurs, the most likely problem is that the
      ;; subscription wasn't cancelled before the connection was
      ;; returned to the pool.
      (warn "Consumer tag of message does not match subscription. tag=~s, subscription=~s"
            consumer subscription-consumer-tag)
      (return-from process-message nil))
    (let ((exchange (cl-rabbit:envelope/exchange msg)))
      (log:trace "Processing message on exchange: ~s" exchange)
      (string-case:string-case (exchange :default nil)
        (#.*channel-content-exchange-name*          (process-channel-message msg msg-formatter))
        (#.*state-server-sender-exchange-name*      (process-state-server-message msg))
        (#.*user-notifications-exchange-name*       (process-user-notification-message msg))
        (#.*unread-state-exchange-name*             (process-unread-message msg))
        (#.*channel-exchange-name*                  (potato.rabbitmq-channels:process-channel-update msg))
        (#.*session-notifications-exchange-name*    (process-session-notification msg))))))

(potato.core:define-json-handler-fn-login (update-active-screen "/update_active" data nil ())
  (potato.core:with-authenticated-user ()
    (json-bind ((cid "channel"))
        data
      (let ((channel (potato.core:load-channel-with-check cid)))
        (potato.core:refresh-user (potato.core:current-user) channel (* *ping-interval* 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Web handlers -- eventsource
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition request-eventsource-poll (lofn:request-polling)
  ((user     :type potato.core:user
             :initarg :user
             :reader request-eventsource-poll/user)
   (channel  :type potato.core:channel
             :initarg :channel
             :reader request-eventsource-poll/channel)
   (queue    :type (or null string)
             :initarg :queue
             :reader request-eventsource-poll/queue)
   (stream   :type stream
             :initarg :stream
             :reader request-eventsource-poll/stream)
   (services :type list
             :initarg :services
             :reader request-eventsource-poll/services)
   (session  :type user-session
             :initarg :session
             :reader request-eventsource-poll/session))
  (:documentation "Lofn request-poll condition that is used when starting the eventsource poll loop"))

(defmethod print-object ((obj request-eventsource-poll) stream)
  (print-unreadable-safely (queue) obj stream
    (let ((user (and (slot-boundp obj 'user) (slot-value obj 'user)))
          (channel (and (slot-boundp obj 'channel) (slot-value obj 'channel))))
      (format stream "USER ~s ~s CHANNEL ~s QUEUE ~s"
              (if user (potato.core:user/id user) :not-bound)
              (if user (potato.core:user/description user) :not-bound)
              (if channel (potato.core:channel/id channel) :not-bound)
              queue))))

(defmethod lofn:start-poll ((condition request-eventsource-poll) socket)
  (let* ((http-event (request-eventsource-poll/queue condition))
         (stream (request-eventsource-poll/stream condition))
         (user (request-eventsource-poll/user condition))
         (channel (request-eventsource-poll/channel condition))
         (services (request-eventsource-poll/services condition))
         (session (request-eventsource-poll/session condition))
         (last-session-refresh 0)
         (consumer-tag nil)
         (mqueue (make-instance 'msgl))
         (timer nil)
         (lock (bordeaux-threads:make-lock))
         (active t)
         (active-eventsource-data (make-active-eventsource-socket-data :request condition :socket socket)))

    (macrolet ((with-msgl-task (&body body)
                 `(msgl-push mqueue (lambda ()
                                      (handler-case
                                          (progn ,@body)
                                        (#+sbcl sb-int:closed-stream-error #-sbcl error (condition)
                                          (log:warn "Error while writing to client: ~a" condition)
                                          (msgl-stop-queue)))))))

      (labels ((refresh-web-session ()
                 (let ((now (get-universal-time)))
                   ;; We only need to do a session refresh every 15 minutes
                   (when (> now (+ last-session-refresh *session-refresh-interval*))
                     (potato.core::update-persisted-session-data session)
                     (setq last-session-refresh now))))

               (send-ping-message ()
                 (with-msgl-task
                     (when (bordeaux-threads:with-lock-held (lock)
                             active)
                       (potato.core:refresh-user user channel (* *ping-interval* 2))
                       (refresh-web-session)
                       (log:trace "Writing keepalive message to client for channel: ~s, user: ~s (~a)"
                                  (potato.core:channel/id channel)
                                  (potato.core:user/id user)
                                  (potato.core:user/description user))
                       (bordeaux-threads:with-lock-held (lock)
                         (format stream ":none~a~a" +nl+ +nl+)
                         (finish-output stream)
                         (setq timer nil)
                         (when active
                           (schedule-notification-timer))))))

               ;; Note: this function writes directly to the stream,
               ;; and must only be called before any of the message
               ;; handlers have been installed.
               (send-error (msg)
                 (log:warn "Sending error on event channel: ~s" msg)
                 (format stream "data:")
                 (st-json:write-json (st-json:jso "error" msg) stream)
                 (format stream "~a~a" +nl+ +nl+)
                 (finish-output stream))

               (handle-incoming-msg (msg)
                 (with-msgl-task
                     (let ((potato.core::*current-auth-user* user))
                       (let ((n (process-message
                                 msg
                                 (bordeaux-threads:with-lock-held (lock)
                                   consumer-tag)
                                 #'potato.core:notification-message-cd->json-html)))
                         (when n
                           (log:trace "Writing data to stream: ~s" n)
                           (bordeaux-threads:with-lock-held (lock)
                             (format stream "data:")
                             (st-json:write-json n stream)
                             (format stream "~a~a" +nl+ +nl+)
                             (finish-output stream)))))))

               (handle-channel-closed (channel)
                 (declare (ignore channel))
                 (when (bordeaux-threads:with-lock-held (lock)
                         active)
                   (log:warn "rabbitmq channel closed while connection is opened")
                   (send-error "queue_error")
                   (usocket:socket-close socket)))

               (schedule-notification-timer ()
                 (when timer
                   (potato.common.timer:unschedule-timer *notifications-timer-queue* timer))
                 (setq timer (potato.common.timer:schedule-timer *notifications-timer-queue*
                                                                 *ping-interval*
                                                                 #'send-ping-message))))

        ;; This is purely for debugging purposes so that we can find the
        ;; list of active connections when using the debugger
        (dhs-sequences:with-cas-update (v *active-eventsource-sockets*)
          (fset:with v active-eventsource-data))

        (unwind-protect
             (let ((rchannel (cl-rabbit-async:multi-connection-open-channel
                              *rabbitmq-multi-connection-instance*
                              :message-callback #'handle-incoming-msg
                              :close-callback #'handle-channel-closed)))

               (unwind-protect
                    (if http-event
                        ;; The client gave us a queue name, check that it exists and is idle
                        (multiple-value-bind (queue-name num-messages num-consumers)
                            (declare-notifications-queue-async rchannel http-event t)
                          (declare (ignore queue-name num-messages))
                          (unless (zerop num-consumers)
                            (error "Queue is not idle: ~s" http-event)))
                        ;; ELSE: Need to declare a new queue
                        (progn
                          (log:warn "No support for client sessions in eventsource implementation")
                          (let ((queue (create-and-bind-notifications-queue-async rchannel user (list channel) services nil)))
                            (setq http-event queue)
                            (format stream "id:~a~a~a" queue +nl+ +nl+)
                            (finish-output stream))))
                 (potato.core:refresh-user user channel (* *ping-interval* 2))
                 (bordeaux-threads:with-lock-held (lock)
                   (let ((v (cl-rabbit-async:async-basic-consume rchannel http-event :no-ack t)))
                     (setq consumer-tag v)))
                 (bordeaux-threads:with-lock-held (lock)
                   (schedule-notification-timer))
                 (loop
                   for finished-p = (multiple-value-bind (sockets remaining)
                                        (usocket:wait-for-input (list socket) :ready-only t)
                                      (declare (ignore remaining))
                                      (member socket sockets))
                   until finished-p)
                 (log:trace "Connection dropped")

                 ;; Unwind form
                 (log:trace "Closing channel: ~s" rchannel)
                 ;; Remove the connection information from the list of active connections
                 (dhs-sequences:with-cas-update (v *active-eventsource-sockets*)
                   (fset:less v active-eventsource-data))
                 ;; Ensure that the ping timer is disabled, and mark the connection as inactive
                 (bordeaux-threads:with-lock-held (lock)
                   (when timer
                     (potato.common.timer:unschedule-timer *notifications-timer-queue* timer)
                     (setq timer nil))
                   (setq active nil))
                 ;; Close the channel (the close-callback for the channel
                 ;; will not do anything at this point since the ACTIVE
                 ;; variable has been set to nil above)
                 (handler-case
                     (cl-rabbit-async:close-channel rchannel)
                   (error (condition)
                     (log:error "Error when closing rabbitmq channel: ~a" condition)))
                 ;; Send a signout message to the state server. This will remove the user after a short delay.
                 (potato.core:signout-user user (potato.core:channel/id channel))))
          ;; UNWIND FORM: No matter what other problems may have
          ;; occurred, we never want to leak a socket.
          (close stream))))))

(potato.core:define-handler-fn-login (channel-updates5 "/chat_updates5/([a-z0-9]+)" t (cid))
  (potato.core:with-authenticated-user ()
    (let* ((user (potato.core:current-user))
           (channel (potato.core:load-channel-with-check cid))
           (http-event (hunchentoot:header-in* :last-event-id)))
      (setf (hunchentoot:header-out :cache-control) "no-cache")
      (setf (hunchentoot:content-type*) "text/event-stream")
      (let ((stream (flexi-streams:make-flexi-stream (hunchentoot:send-headers)
                                                     :external-format :utf-8)))
        (verify-queue-name http-event (list (potato.core:channel/id channel)))
        (signal 'request-eventsource-poll
                :user user
                :channel channel
                :queue http-event
                :stream stream
                :services +all-services+
                :session (potato.core:find-current-user-session))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Web handlers -- long poll
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun consume-message-handle-timeout (conn timeout-secs)
  (handler-bind ((cl-rabbit:rabbitmq-library-error (lambda (cond)
                                                     (when (eq (cl-rabbit:rabbitmq-library-error/error-code cond)
                                                               :amqp-status-timeout)
                                                       (return-from consume-message-handle-timeout nil)))))
    (cl-rabbit:consume-message conn :timeout (truncate (* timeout-secs 1000 1000)))))

(defun poll-notifications (conn subscription-consumer-tag num-messages
                           &key (timeout 30) (msg-formatter #'potato.core:notification-message-cd->json-html))
  "Waits for messages from RabbitMQ and returns a list of processed
messages. Each element is a list of two values: the processed message
and the delivery tag."
  (let ((existing-messages (loop
                             repeat num-messages
                             for msg = (consume-message-handle-timeout conn 1/1000)
                             while msg
                             for json = (process-message msg subscription-consumer-tag msg-formatter)
                             if json
                               collect (list json (cl-rabbit:envelope/delivery-tag msg))
                             else do (cl-rabbit:basic-ack conn 1 (cl-rabbit:envelope/delivery-tag msg)))))
    (or existing-messages
        (alexandria:when-let ((msg (consume-message-handle-timeout conn timeout)))
          (alexandria:when-let ((json (process-message msg subscription-consumer-tag msg-formatter)))
            (list (list json (cl-rabbit:envelope/delivery-tag msg))))))))

(define-condition request-long-poll (lofn:request-polling)
  ((user              :type potato.core:user
                      :initarg :user
                      :reader request-long-poll/user)
   (channels          :type list
                      :initarg :channels
                      :reader request-long-poll/channels)
   (queue             :type (or null string)
                      :initarg :queue
                      :reader request-long-poll/queue)
   (stream            :type stream
                      :initarg :stream
                      :reader request-long-poll/stream)
   (services          :type list
                      :initarg :services
                      :reader request-long-poll/services)
   (msg-formatter     :type function
                      :initform #'potato.core:notification-message-cd->json-html
                      :initarg :msg-formatter
                      :reader request-long-poll/msg-formatter)
   (json-formatter-fn :type function
                      :initarg :json-formatter-fn
                      :reader request-long-poll/json-formatter-fn)
   (client-session    :type (or null string)
                      :initarg :client-session
                      :reader request-long-poll/client-session)
   (active-p          :type t
                      :initarg :active-p
                      :reader request-long-poll/active-p))
  (:documentation "Lofn request-poll condition that is used when starting the long poll loop"))

(defmethod lofn:start-poll ((condition request-long-poll) socket)
  (let ((user          (request-long-poll/user              condition))
        (stream        (request-long-poll/stream            condition))
        (channels      (request-long-poll/channels          condition))
        (event         (request-long-poll/queue             condition))
        (msg-formatter (request-long-poll/msg-formatter     condition))
        (fmt-fn        (request-long-poll/json-formatter-fn condition))
        (services      (request-long-poll/services          condition))
        (sid           (request-long-poll/client-session    condition))
        (active-p      (request-long-poll/active-p          condition))
        (result-written-p nil)
        (timeout       30))

    (labels ((write-result (data)
               (setq result-written-p t)
               (st-json:write-json data stream)
               (finish-output stream)))
      (unwind-protect
           (block process-poll
             (let ((potato.core::*current-auth-user* user))
               (with-rabbitmq-connected (conn)
                 (multiple-value-bind (name num-messages)
                     (if event
                         (handler-bind ((cl-rabbit:rabbitmq-server-error
                                          (lambda (condition)
                                            (when (= (cl-rabbit:rabbitmq-server-error/reply-code condition) 404)
                                              (write-result (st-json:jso "result" "error"
                                                                         "detail" "unknown_event"
                                                                         "message" "Illegal event"))
                                              (return-from process-poll nil)))))
                           (multiple-value-bind (queue-name num-messages num-consumers)
                               (declare-notifications-queue conn 1 event t)
                             (declare (ignore queue-name))
                             (unless (zerop num-consumers)
                               (error "Queue is not idle: ~s" event))
                             (values event num-messages)))
                         (let ((n (create-and-bind-notifications-queue conn 1
                                                                       (potato.core:current-user)
                                                                       channels services sid)))
                           (values n 0)))
                   (when active-p
                     (dolist (channel channels)
                       (potato.core:refresh-user user channel (* *ping-interval* 2))))
                   (unwind-protect
                        (let ((consumer-tag (cl-rabbit:basic-consume conn 1 name :no-ack nil)))
                          (log:trace "Created consumer tag: ~s, queue: ~s" consumer-tag name)
                          (unwind-protect
                               (let ((result (loop
                                               with timeout-time = (+ (get-universal-time) timeout)
                                               for tm = (- timeout-time (get-universal-time))
                                               while (plusp tm)
                                               for msglist = (poll-notifications conn consumer-tag num-messages
                                                                                 :msg-formatter msg-formatter :timeout tm)
                                               until msglist
                                               finally (return msglist))))
                                 (write-result (funcall fmt-fn name (mapcar #'first result)))
                                 ;; At this point, it should be reasonably safe to ack the messages
                                 (mapc (lambda (v) (cl-rabbit:basic-ack conn 1 (second v))) result))
                            ;; UNWIND FORM: Cancel the subscription
                            (cl-rabbit:basic-cancel conn 1 consumer-tag)))
                     ;; UNWIND FORM: Make sure user is logged out
                     (when active-p
                       (dolist (channel channels)
                         (potato.core:signout-user user channel))))))))
        ;; UNWIND FORM: Don't leak a file descriptor
        (unwind-protect
             (unless result-written-p
               (st-json:write-json (st-json:jso "result" "error"
                                                "detail" "internal_error"
                                                "message" "Internal error, probably incorrect event-id")
                                   stream))
          (close stream))))))

(defun process-long-poll (channels event sid services msg-formatter json-formatter-fn active-p)
  (let ((stream (flexi-streams:make-flexi-stream (hunchentoot:send-headers) :external-format :utf-8)))
    (signal 'request-long-poll
            :user (potato.core:current-user)
            :channels channels
            :queue event
            :stream stream
            :services services
            :msg-formatter msg-formatter
            :json-formatter-fn json-formatter-fn
            :client-session sid
            :active-p active-p)))

(lofn:define-handler-fn (channel-updates6 "/chat_updates6" nil ())
  (handler-case
      (potato.core:with-authenticated-user ()
        (json-bind ((cid "channel")
                    (event "connection" :required nil)
                    (sid "session-id" :required nil)
                    (active-p "is-active" :required nil :type :boolean))
            (st-json:read-json-from-string (hunchentoot:raw-post-data :force-text t))
          (let ((channel (potato.core:load-channel-with-check cid)))
            (verify-queue-name event (list (potato.core:channel/id channel)))
            (process-long-poll (list channel) event sid
                               +all-services+
                               #'potato.core:notification-message-cd->json-html
                               (lambda (queue notifications)
                                 (st-json:jso "connection" queue
                                              "data" notifications))
                               active-p))))
    (potato.core:potato-error (condition)
      (setf (hunchentoot:return-code*) (potato.core:potato-error/response-status condition))
      "User is not logged in")))
