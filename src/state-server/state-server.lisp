(in-package :state-server)

(declaim #.potato.common::*compile-decl*)

(defvar *channel-state-list* (make-fsmap))

(defvar *default-user-logout-time* 60
  "The number of seconds until a user is logged off the channel until
a refresh message was sent.")

(defvar *sender-rchannel* nil)
(defvar *timer-queue* (potato.common.timer:make-timer-queue))
(defvar *msgl-queue* (make-instance 'msgl))

;;;
;;;  Format of the routing key on *state-server-sender-exchange-name*
;;;
;;;    category.command.channel.user.recipient
;;;
;;;    category: "change" or "sync":
;;;          change indicates an update of the current state, i.e. add/remove or typing commands
;;;          sync means that the command is a full state synchronisation
;;;
;;;    command: the command name. This is the first element in the actual message content.
;;;
;;;    channel: the channel the command refers to
;;;
;;;    user: the user-id of the user that is affected by the command, or "sync" for the full sync command
;;;
;;;    recipient: "all" for general messages, or the user id of the recipient for sync commands that
;;;               are directed to a specific user
;;;

(defclass channel-state ()
  ((id         :type string
               :initarg :id
               :reader channel-state/id
               :documentation "ID of this channel")
   (users      :type dhs-sequences:blocking-hash-map
               :accessor channel-state/users
               :documentation "List of user in the channel. Keyed on user-id, contains an instance of CHANNEL-USER"))
  (:documentation "State for a single channel"))

(defmethod initialize-instance :after ((obj channel-state) &key)
  (unless (slot-boundp obj 'users)
    (setf (channel-state/users obj)
          (dhs-sequences:make-blocking-hash-map :test 'equal
                                                :name (format nil "User list for channel ~s"
                                                              (channel-state/id obj))))))

(defclass channel-user ()
  ((id           :type string
                 :initarg :id
                 :reader channel-user/id)
   (channel      :type channel-state
                 :initarg :channel
                 :reader channel-user/channel)
   (timer        :type (or null potato.common.timer:timer)
                 :initform nil
                 :accessor channel-user/timer)
   (text-timer   :type (or null potato.common.timer:timer)
                 :initform nil
                 :accessor channel-user/text-timer)
   (text-p       :type t
                 :initform nil
                 :accessor channel-user/text-p)
   (lock         :type t
                 :reader channel-user/lock))
  (:documentation "State for a user in a channel"))

(defmethod initialize-instance :after ((obj channel-user) &key &allow-other-keys)
  (setf (slot-value obj 'lock) (bordeaux-threads:make-lock (format nil "User state lock for channel=~a, user=~a"
                                                                   (channel-state/id (channel-user/channel obj))
                                                                   (channel-user/id obj)))))

(defun remove-user (ch uid)
  (log:trace "Removing user ~s from channel ~s" uid (channel-state/id ch))
  (let ((users (channel-state/users ch)))
    (dhs-sequences:with-locked-instance users
      (let ((user (dhs-sequences:hash-get users uid)))
        (when user
          (dhs-sequences:hash-remove users uid)
          (with-msgl *msgl-queue*
            (let ((routing-key (format nil "change.~a.~a.~a.all"
                                       *state-server-reply-user-remove-from-channel*
                                       (channel-state/id ch)
                                       (encode-name-for-routing-key uid)))
                  (body (lisp-to-binary (list *state-server-reply-user-remove-from-channel*
                                              (channel-state/id ch) uid))))
              (cl-rabbit-async:async-basic-publish *sender-rchannel*
                                                   :exchange *state-server-sender-exchange-name*
                                                   :routing-key routing-key
                                                   :body body))))))))

(defun get-channel-state (channel-id &key (create-if-missing t))
  (or (fsmap-value *channel-state-list* channel-id)
      (if create-if-missing
          (fsmap-set *channel-state-list* channel-id (make-instance 'channel-state :id channel-id) :no-replace t))))

(defun send-add-to-queue (cid uid)
  (with-msgl *msgl-queue*
    (cl-rabbit-async:async-basic-publish *sender-rchannel*
                                         :exchange *state-server-sender-exchange-name*
                                         :routing-key (format nil "change.~a.~a.~a.all"
                                                              *state-server-reply-user-add-to-channel*
                                                              cid (encode-name-for-routing-key uid))
                                         :body (lisp-to-binary (list *state-server-reply-user-add-to-channel*
                                                                     cid uid)))))

(defun %add-user (channel-id user-id timeout)
  (log:trace "adding user, channel: ~s, user: ~s, timeout: ~s" channel-id user-id timeout)
  (let ((channel-state (get-channel-state channel-id)))
    (let ((will-send nil))
      (let ((user-state (dhs-sequences:with-hash-get-or-update (channel-state/users channel-state) user-id
                          (let ((u (make-instance 'channel-user :id user-id :channel channel-state)))
                            (setq will-send t)
                            u))))
        (when will-send
          (send-add-to-queue (channel-state/id channel-state)
                             (channel-user/id user-state)))
        (bordeaux-threads:with-lock-held ((channel-user/lock user-state))
          (with-accessors ((timer channel-user/timer)) user-state
            (when timer
              (potato.common.timer:unschedule-timer *timer-queue* timer))
            (setf timer
                  (potato.common.timer:schedule-timer *timer-queue* timeout
                                                      (lambda ()
                                                        (log:trace "removing user because of timer expiry: ~s" user-id)
                                                        (lparallel:future
                                                          (remove-user channel-state user-id)))))))))))

(defun add-user (channel-id user-id)
  (%add-user channel-id user-id *default-user-logout-time*))

(defun handle-channel-remove-with-timeout (channel-id user-id timeout)
  (%add-user channel-id user-id timeout))

(defun send-typing-to-queue (channel-id user-id)
  (log:trace "Sending typing message to queue, user-id=~s, channel-id=~s" user-id channel-id)
  (cl-rabbit-async:async-basic-publish *sender-rchannel*
                                       :exchange *state-server-sender-exchange-name*
                                       :routing-key (format nil "change.~a.~a.~a.all"
                                                            *state-server-reply-typing-start*
                                                            channel-id (encode-name-for-routing-key user-id))
                                       :body (lisp-to-binary (list *state-server-reply-typing-start* channel-id user-id))))

(defun typing-end (user-state)
  (let* ((channel-state (channel-user/channel user-state))
         (user-id (channel-user/id user-state))
         (channel-id (channel-state/id channel-state)))
    (log:trace "Sending typing end, user-id=~s, channel-id=~s" user-id channel-id)
    (bordeaux-threads:with-lock-held ((channel-user/lock user-state))
      (setf (channel-user/text-p user-state) nil))
    (cl-rabbit-async:async-basic-publish *sender-rchannel*
                                         :exchange *state-server-sender-exchange-name*
                                         :routing-key (format nil "change.~a.~a.~a.all"
                                                              *state-server-reply-typing-end*
                                                              channel-id (encode-name-for-routing-key user-id))
                                         :body (lisp-to-binary (list *state-server-reply-typing-end* channel-id user-id)))))

(defun handle-typing-begin (channel-id user-id)
  (log:trace "State server received typing-begin message. channel=~s, user=~s" channel-id user-id)
  (alexandria:when-let ((channel-state (get-channel-state channel-id :create-if-missing nil)))
    (alexandria:when-let ((user-state (dhs-sequences:hash-get (channel-state/users channel-state) user-id)))
      (let ((will-send nil))
        (bordeaux-threads:with-lock-held ((channel-user/lock user-state))
          (unless (channel-user/text-p user-state)
            (setq will-send t)
            (setf (channel-user/text-p user-state) t)))
        (when will-send
          (send-typing-to-queue channel-id user-id))
        (bordeaux-threads:with-lock-held ((channel-user/lock user-state))
          (with-accessors ((timer channel-user/text-timer)) user-state
            (when timer
              (potato.common.timer:unschedule-timer *timer-queue* timer))
            (setf timer
                  (potato.common.timer:schedule-timer *timer-queue* 4
                                                      (lambda ()
                                                        (lparallel:future
                                                          (typing-end user-state)))))))))))

(defun handle-typing-end (channel-id user-id)
  (log:trace "State server received typing-begin message. channel=~s, user=~s" channel-id user-id)
  (alexandria:when-let ((channel-state (get-channel-state channel-id :create-if-missing nil)))
    (alexandria:when-let ((user-state (dhs-sequences:hash-get (channel-state/users channel-state) user-id)))
      (let ((will-send nil))
        (bordeaux-threads:with-lock-held ((channel-user/lock user-state))
          (when (channel-user/text-p user-state)
            (with-accessors ((timer channel-user/text-timer)) user-state
              (when timer
                (potato.common.timer:unschedule-timer *timer-queue* timer)
                (setf timer nil)))
            (setq will-send t)))
        (when will-send
          (typing-end user-state))))))

(defun send-full-user-list-to-rabbitmq-queue (channel-id users recipient)
  (let ((formatted (mapcar (lambda (user)
                             (list (channel-user/id user)))
                           users)))
    (with-msgl *msgl-queue*
      (cl-rabbit-async:async-basic-publish *sender-rchannel*
                                           :exchange *state-server-sender-exchange-name*
                                           :routing-key (format nil "sync.~a.~a.sync.~a"
                                                                *state-server-reply-sync-users*
                                                                channel-id (or (encode-name-for-routing-key recipient) "all"))
                                           :body (lisp-to-binary (list *state-server-reply-sync-users* channel-id formatted))))))

(defun send-full-user-list (channel-id recipient)
  (log:trace "Sending full user list for channel: ~s" channel-id)
  (let ((channel-state (get-channel-state channel-id :create-if-missing t)))
    (when channel-state
      (let* ((users (channel-state/users channel-state))
             (users-copy (dhs-sequences:with-locked-instance users
                           (loop
                              with iterator = (dhs-sequences:hash-iterator users :content :value)
                              for value = (funcall iterator)
                              while value
                              collect value))))
        (send-full-user-list-to-rabbitmq-queue channel-id users-copy recipient)))))

(defun handle-cmd (msg)
  ;; The command is a sexp with the first element being a command:
  ;;
  ;;   These are the possible commands:
  ;;     ("channel" channel-id user-id user-description)
  ;;     ("chtime" channel-id user-id user-description time-in-seconds)
  (let* ((message (cl-rabbit:envelope/message msg))
         (req (binary-to-lisp (cl-rabbit:message/body message))))
    (log:trace "Incoming message to state server: ~s" req)
    (string-case:string-case ((car req))
      (#.*state-server-message-channel-join*
       (destructuring-bind (channel-id user-id) (cdr req)
         (add-user channel-id user-id)))

      (#.*state-server-message-channel-join-with-timeout*
       (destructuring-bind (channel-id user-id time) (cdr req)
         (handle-channel-remove-with-timeout channel-id user-id time)))

      (#.*state-server-message-typing-start*
       (destructuring-bind (channel-id user-id) (cdr req)
         (handle-typing-begin channel-id user-id)))

      (#.*state-server-message-typing-end*
       (destructuring-bind (channel-id user-id) (cdr req)
         (handle-typing-end channel-id user-id)))

      (#.*state-server-message-sync-users*
       (send-full-user-list (second req) (third req))))))

(defun state-server-loop ()
  (setq *sender-rchannel* (cl-rabbit-async:multi-connection-open-channel
                           *rabbitmq-multi-connection-instance*
                           :close-callback (lambda (channel)
                                             (declare (ignore channel))
                                             (log:error "State server channel should never be closed"))))
  (unwind-protect
       (with-rabbitmq-connected (conn)
         (let ((queue-name (cl-rabbit:queue-declare conn 1 :auto-delete t :durable nil)))
           (cl-rabbit:queue-bind conn 1
                                 :queue queue-name
                                 :exchange *state-server-reader-exchange-name*
                                 :routing-key "#"
                                 :arguments '(("x-max-priority" . 1)))
           (cl-rabbit:basic-consume conn 1 queue-name :no-ack t)
           (loop
              for msg = (cl-rabbit:consume-message conn)
              do (handle-cmd msg))))
    (let ((ch *sender-rchannel*))
      (setq *sender-rchannel* nil)
      (cl-rabbit-async:close-channel ch))))

(potato.common.application:define-component state-server
  (:dependencies potato.common::generic)
  (:start
   (setq *timer-queue* (potato.common.timer:make-timer-queue :name "Timer queue for state server"))
   (start-monitored-thread #'state-server-loop "State server main")
   (log:info "State server started")))
