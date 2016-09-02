(in-package :potato.core)

(declaim #.potato.common::*compile-decl*)

(defparameter *signout-delay-time* 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Old channel state sync code that can't be removed yet since the
;;; unread state handling needs to gain access to this information.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *state-sources* (receptacle:make-blocking-hash-map :test 'equal))

(defvar *state-server-connection* nil)
(defvar *state-server-queue-name* nil)

(defclass state-source ()
  ((channel-id    :type string
                  :initarg :channel-id
                  :reader state-source/channel-id)
   (members       :type receptacle.red-black-tree:red-black-tree
                  :initform (make-instance 'receptacle.red-black-tree:red-black-tree
                                           :test #'string< :test-equal #'string= :key #'identity)
                  :reader state-source/members)
   (sync-callback :type (or null function)
                  :initarg :sync-callback
                  :initform nil
                  :accessor state-source/sync-callback
                  :documentation "Function that is called at the first full sync of the user list")
   (lock          :type t
                  :initform (bordeaux-threads:make-lock "State server lock")
                  :reader state-source/lock))
  (:documentation "Source that tracks the users in a channel"))

(defmethod print-object ((obj state-source) stream)
  (print-unreadable-safely (channel-id) obj stream
    (format stream "CHANNEL-ID ~s" channel-id)))

(defun notify-user-join-remove (mode channel-id user-id)
  (log:trace "Notifying user join or remove. mode=~s, channel=~s, user=~s" mode channel-id user-id)
  (alexandria:when-let ((state-source (find-state-source channel-id :create nil)))
    (bordeaux-threads:with-lock-held ((state-source/lock state-source))
      (let ((members (state-source/members state-source)))
        (ecase mode
          (:add (receptacle:tree-insert members user-id))
          (:remove (receptacle::tree-delete-element members user-id)))))))

(defun process-sync-users (channel-id users)
  (log:trace "Performing full sync of channel members. channel=~s, users=~s" channel-id users)
  (alexandria:when-let ((state-source (find-state-source channel-id :create nil)))
    (let ((callback nil))
      (bordeaux-threads:with-lock-held ((state-source/lock state-source))
        (let ((members (state-source/members state-source)))
          (receptacle:delete-all members)
          (dolist (user users)
            (receptacle:tree-insert members (first user)))
          ;; Check if SYNC-CALLBACK is set. If it is, we copy the
          ;; value before clearing the slot. The function will be
          ;; called later, outside the locked section.
          (when (state-source/sync-callback state-source)
            (setq callback (state-source/sync-callback state-source))
            (setf (state-source/sync-callback state-source) nil))))
      ;; Call the sync callback if it had been set
      (when callback
        (funcall callback)))))

(defun dispatch-channel-changes-message (msg)
  (let ((req (binary-to-lisp (cl-rabbit:message/body (cl-rabbit:envelope/message msg)))))
    (log:trace "State server message: ~s" req)
    (string-case:string-case ((car req))
      (#.*state-server-reply-user-add-to-channel*
       (destructuring-bind (channel-id user-id) (cdr req)
         (notify-user-join-remove :add channel-id user-id)))

      (#.*state-server-reply-user-remove-from-channel*
       (destructuring-bind (channel-id user-id) (cdr req)
         (notify-user-join-remove :remove channel-id user-id)))

      (#.*state-server-reply-sync-users*
       (destructuring-bind (channel-id users) (cdr req)
         (process-sync-users channel-id users)))

      (t nil))))

(defun state-listener ()
  (with-rabbitmq-connected (conn)
    (let ((queue-name (cl-rabbit:queue-declare conn 1 :auto-delete t :durable nil :exclusive nil)))
      #+nil(cl-rabbit:queue-bind conn 1 :queue queue-name :exchange *state-server-sender-exchange-name* :routing-key "#")
      (cl-rabbit:basic-consume conn 1 queue-name :no-ack t)
      (setq *state-server-queue-name* queue-name)
      (loop
         for msg = (cl-rabbit:consume-message conn)
         do (dispatch-channel-changes-message msg)))))

(defun register-source-with-state-server (channel-id)
  (with-pooled-rabbitmq-connection (conn)
    (if *state-server-queue-name*
        (cl-rabbit:queue-bind conn 1
                              :queue *state-server-queue-name*
                              :exchange *state-server-sender-exchange-name*
                              :routing-key (format nil "*.*.~a.*.all" channel-id))
        ;; ELSE: No queue name set, log an error
        (log:error "No state server queue name set"))
    (send-rabbitmq-message-to-state-server channel-id t (list *state-server-message-sync-users* channel-id nil)
                                           :connection conn)))

(defun find-state-source (channel-id &key create sync-callback)
  "Find the state source for CHANNEL-ID and return the source and
NIL. If the state source does not exist and CREATE is non-nil,
create a new state source, and return the new source and T. If The
source does not exist and CREATE is NIL, return NIL and NIL.

SYNC-CALLBACK can be used to specify the callback function to pass to
the creation of the state source when creating a new instance."
  (check-type channel-id string)
  (if create
      (receptacle:with-hash-get-or-update *state-sources* channel-id
        (let ((source (make-instance 'state-source :channel-id channel-id :sync-callback sync-callback)))
          (register-source-with-state-server channel-id)
          source))
      (values (receptacle:hash-get *state-sources* channel-id) nil)))

(defun start-state-listener-thread ()
  (start-monitored-thread #'state-listener "State listener loop"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RabbitMQ notifications code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun notification-message-cd->json-html (v)
  (message-cd->json v :html (current-user)))

(defun notification-message-cd->json-alist (v)
  (message-cd->json v :alist (current-user)))

(defun notification-message-cd->json-text (v)
  (message-cd->json v :text (current-user)))

(defun refresh-user (user channel delay-time)
  (check-type user user)
  (let ((channel-id (ensure-channel-id channel)))
    (log:trace "Refreshing user state for state server: ~s, channel-id=~s" (user/id user) channel-id)
    (send-rabbitmq-message-to-state-server channel-id nil (list *state-server-message-channel-join-with-timeout*
                                                                channel-id (user/id user) delay-time))
    (clear-unread-state-for-user-in-channel (user/id user) channel-id)))

(defun signout-user (user channel)
  (check-type user user)
  (let ((channel-id (ensure-channel-id channel)))
    (log:trace "Signout user: ~s, channel-id=~s" (user/id user) channel-id)
    (send-rabbitmq-message-to-state-server channel-id nil (list *state-server-message-channel-join-with-timeout*
                                                                channel-id (user/id user)
                                                                *signout-delay-time*))))
