(in-package :potato.core)

(declaim #.potato.common::*compile-decl*)

(defclass user-unread-channel-state ()
  ((channels :type list
             :initarg :unread-channels
             :initform nil
             :accessor user-unread-channel-state/unread-channels))
  (:documentation "Object holding the cached unread state for the channels of a single user."))

(defclass user-unread-state-rabbitmq-message ()
  ((user    :type string
            :initarg :user
            :reader user-unread-state-rabbitmq-message/user)
   (channel :type string
            :initarg :channel
            :reader user-unread-state-rabbitmq-message/channel)
   (count   :type integer
            :initarg :count
            :reader user-unread-state-rabbitmq-message/count)))

(defun make-memcached-key-for-unread-state (user-id)
  (format nil "unread-channels-~a" user-id))

(defun get-cached-unread-state-for-user-if-exists (user)
  "If the unread state is cached for USER-ID, then return the cached
information. Otherwise return NIL."
  (let* ((user-id (ensure-user-id user))
         (memcached-key (make-memcached-key-for-unread-state user-id)))
    (potato.common.memcached:find-cached-object-if-exists memcached-key)))

(defun unread-channels-for-user (user &key ignore-cache)
  (let* ((user-id (ensure-user-id user))
         (memcached-key (make-memcached-key-for-unread-state user-id))
         (state (and (not ignore-cache)
                     (potato.common.memcached:find-cached-object-if-exists memcached-key))))
    (if state
        (user-unread-channel-state/unread-channels state)
        ;; ELSE: No cached state, load it from couchdb and update cache
        (let* ((result (clouchdb:invoke-view "channel" "recent_message"
                                             :start-key (list user nil)
                                             :end-key (list user 'clouchdb:json-map)))
               (n (make-instance 'user-unread-channel-state
                                 :unread-channels (mapcar #'(lambda (row)
                                                              (second (getfield :|key| row))
                                                              #+nil(let ((value (getfield :|value| row)))
                                                                     (list (second (getfield :|key| row))
                                                                           (getfield :|count| value)
                                                                           (getfield :|last_read| value))))
                                                          (getfield :|rows| result)))))
          (cl-memcached:mc-set memcached-key (conspack:encode n))
          (user-unread-channel-state/unread-channels n)))))

(defun flush-cached-user-channel-state (user)
  (cl-memcached:mc-del (make-memcached-key-for-unread-state (ensure-user-id user))))

(defun push-user-channel-notification (user-id-list channel-id)
  "Send a global notification indicating that the unread state for a
set of users have changed. USER-ID-LIST is a list of lists. Each list
consists of two elements: the user id and the new unread count for the
user. CHANNEL-ID is the id of the channel for which this update
applies."
  (with-pooled-rabbitmq-connection (conn)
    ;; First, send one notification to each user in the list. These
    ;; are the messages that are picked up by the queue handler that
    ;; pushes a message to an active web client.
    (dolist (entry user-id-list)
      (let* ((uid (first entry))
             (msg (make-instance 'user-unread-state-rabbitmq-message
                                 :user uid
                                 :count (second entry)
                                 :channel channel-id)))
        (cl-rabbit:basic-publish conn 1
                                 :exchange *unread-state-exchange-name*
                                 :routing-key (format nil "~a.~a" (encode-name-for-routing-key uid) channel-id)
                                 :body (conspack:encode msg))))
    ;; Next, send a single message to a separate queue that is read by
    ;; the GCM handler.
    (cl-rabbit:basic-publish conn 1
                             :exchange *gcm-unread-state-exchange-name*
                             :routing-key channel-id
                             :body (lisp-to-binary (list channel-id user-id-list)))))

(defun update-unread-state-after-post-message (mapping)
  "After a message is posted on a channel, this function increments
the unread count for each non-active user in the channel, and sends a
global notification to all users that for whom the unread count
transitioned from 0 to 1.

This function alters the MAPPING argument with the new activate state.
Note that this value is not necessarily identical to the value that is
being stored in the database. This is because of an optimisation that
sometimes avoids updating the database if the list of users with
\(unread > 0) has not changed."
  (check-type mapping channel-users)
  (let* ((cid (potato.core:channel-users/channel-id mapping)))
    (multiple-value-bind (state-source created-p)
        (find-state-source cid :create t :sync-callback (lambda () (update-unread-state-after-post-message mapping)))
      (when (and state-source (not created-p))
        (let ((active-users (state-source/members state-source)))
          (multiple-value-bind (updated needs-update)
              (loop
                 for element in (channel-users/users mapping)
                 for user = (car element)
                 for user-string = (string user)
                 for values = (cdr element)
                 for count = (getfield :|count| values)
                 for user-is-active-p = (and active-users (receptacle:tree-find-element active-users user-string))
                 for hidden = (getfield :|hide| values :accept-missing t)
                 for user-is-updated-p = (or hidden
                                             (and (zerop count)
                                                  (not user-is-active-p)))
                 when user-is-updated-p
                 collect (list user-string (1+ count)) into needs-update
                 collect `(,user . ((:|count|     . ,(if user-is-active-p 0 (1+ count)))
                                    (:|last_read| . ,(getfield :|last_read| values))))
                 into updated
                 finally (return (values updated needs-update)))
            (handler-case
                ;; As an optimisation, we only update the document if there
                ;; are any users that needs update. This means that the
                ;; number of times the document gets updates is drastically
                ;; reduced, but with the consequence that the only reliable
                ;; values for unread-count is 0 and 1. In order to
                ;; accurately track the unread count, the below form needs
                ;; to be called at all times, not just when needs-update is
                ;; true.
                (when needs-update
                  (setf (channel-users/users mapping) updated)
                  (potato.db:save-instance mapping)
                  (loop
                     for (user-id count) in needs-update
                     do (log:trace "need update, user=~s channel=~s" user-id cid)
                     do (flush-cached-user-channel-state user-id))
                  (push-user-channel-notification needs-update cid))
              ;; If there is a revision conflict here, that means that there is another message
              ;; being sent at exactly the same time. In this case, we'll simply skip the update,
              ;; since the only negative effect is that the unread count will be too low, which is
              ;; perfectly acceptable.
              (clouchdb:id-or-revision-conflict () (log:warn "Revision conflict while updating unread state")))))))))

(defun clear-unread-state-for-user-in-channel (user channel)
  "Update the unread count and last update time for USER as zero in
CHANNEL. If the unread count was previously non-zero, publish a global
notification."
  (let ((user-id (ensure-user-id user))
        (channel-id (ensure-channel-id channel)))
    (log:trace "Clearing unread count for user ~s in channel ~s" user-id channel-id)
    ;; First, let's check if the unread state is cached in memcached for this user
    (let ((cached (get-cached-unread-state-for-user-if-exists user-id)))
      (if (and cached
               (not (member channel-id (user-unread-channel-state/unread-channels cached) :test #'equal)))
          ;; There is already an entry in memcached indicating that the user doesn't have
          ;; any unread messaged in this channel, so we don't have to do anything further.
          ;;
          ;; In the future, we might want to keep track of the "last read" date here, but
          ;; for now, we'll just log a trace message for good measure.
          (log:trace "Channel is already marked as unread. user=~s, channel=~s" user-id channel-id)
          ;; ELSE: we don't have a cached entry, so we might need to update the state in couchdb.
          ;; This is done using an update handler.
          (let* ((mapping-id (channel-users-mapping-name channel-id))
                 (result (potato.db:call-clouchdb-update-function "channel" "mark_read"
                                                                  mapping-id
                                                                  `(("user_id" . ,user-id)
                                                                    ("date"    . ,(format-timestamp nil (local-time:now)))))))
            ;; The channel user mapping has been updated here, so
            ;; clear the memcached state for the object
            (potato.db:flush-cache-for-object-if-enabled (find-class 'channel-users) mapping-id)
            ;; We need to make sure the memcached state is up to date.
            ;;
            ;; The current implementation reuses the result from the memcached entry
            ;; if available. This could potentially introduce a race condition if another
            ;; change (most likely a full flush) has been done since the entry was loaded.
            ;;
            ;; A different way of doing it that eliminates this race condition is to always
            ;; flush the cache and force by calling UNREAD-CHANNELS-FOR-USER.
            (if cached
                ;; We have the previous cached result, so we can update it in-place and save
                ;; the object back to the cache.
                (with-accessors ((channels user-unread-channel-state/unread-channels)) cached
                  (setq channels (remove channel-id channels :test #'equal :count 1))
                  (cl-memcached:mc-set (make-memcached-key-for-unread-state user-id)
                                       (conspack:encode cached)))
                ;; ELSE: There was no cached entry to begin with, so we simply reload the unread
                ;; state with the parameter :IGNORE-CACHE set to true, which will cause the cache
                ;; to be updated.
                (unread-channels-for-user user-id :ignore-cache t))
            ;; The update function handler returns a "count" field containing the number
            ;; of unread messages for this user prior to calling this function. If this
            ;; value is greater than 0, the clients need to be notifed, so we'll send a
            ;; notification.
            (when (plusp (getfield :|count| result))
              (push-user-channel-notification (list (list user-id 0)) channel-id)))))))
