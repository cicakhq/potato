(in-package :potato.core)

(declaim #.potato.common::*compile-decl*)

(defvar *max-message-size* 10000
  "The maximum number of characters in a single message that is posted from a user.")

(defclass channel ()
  ((name     :type string
             :initarg :name
             :accessor channel/name
             :persisted-p t
             :record-changes-p t)
   (topic    :type string
             :initarg :topic
             :accessor channel/topic
             :persisted-p t
             :persisted-allow-missing-value t
             :persisted-missing-default "NIL"
             :record-changes-p t)
   (group    :type string
             :initarg :group
             :reader channel/group
             :persisted-p t)
   (domain   :type string
             :initarg :domain
             :reader channel/domain
             :persisted-p t)
   (deleted  :type t
             :initform nil
             :accessor channel/deleted
             :persisted-p t
             :persisted-type :boolean
             :persisted-allow-missing-value t
             :persisted-missing-default nil))
  (:metaclass potato.db:persisted-entry-class)
  (:memcached-enabled-p t)
  (:documentation "Object describing a channel"))

(defmethod print-object ((obj channel) stream)
  (print-unreadable-safely (name) obj stream
    (format stream "~s ~s" (channel/id obj) name)))

(defgeneric channel/id (channel))
(defmethod channel/id ((channel channel))
  (potato.db:persisted-entry/couchdb-id channel))

(defclass channel-users ()
  ((channel    :type string
               :initarg :channel-id
               :initform (error "~s is required for ~s" :channel-id 'channel-users)
               :reader channel-users/channel-id
               :persisted-p t)
   (name       :type string
               :initarg :name
               :accessor channel-users/name
               :persisted-p t
               :documentation "Mirrors the NAME element in the CHANNEL class")
   (group      :type string
               :initarg :group
               :reader channel-users/group
               :persisted-p t
               :documentation "Mirrors the GROUP element in the CHANNEL class")
   (domain     :type string
               :initarg :domain
               :reader channel-users/domain
               :persisted-p t
               :documentation "Mirrors the DOMAIN element in the CHANNEL class")
   (group-type :type group-type
               :initarg :group-type
               :reader channel-users/group-type
               :persisted-p t
               :persisted-type :symbol
               :documentation "Mirrors the GROUP-TYPE element in the GROUP class")
   (deleted    :type t
               :initform nil
               :accessor channel-users/deleted
               :persisted-p t
               :persisted-type :boolean
               :persisted-allow-missing-value t
               :persisted-missing-default nil
               :documentation "Mirrors the DELETED element in the CHANNEL class")
   (users      :type list
               :initarg :users
               :initform nil
               :accessor channel-users/users
               :persisted-p t
               :persisted-type :json
               :documentation "Map of all users, keyed on user-id, with values: count, last_read, hide"))
  (:metaclass potato.db:persisted-entry-class)
  (:memcached-enabled-p t)
  (:documentation "Object holding information on a channel's members as well as number of unread messages"))

(defmethod print-object ((obj channel-users) stream)
  (print-unreadable-safely (channel name) obj stream
    (format stream "CHANNEL ~s NAME ~s" channel name)))

(defun channel-users-mapping-name (channel)
  (let ((channel-id (ensure-channel-id channel)))
    (concatenate 'string "channel-users-" channel-id)))

(defmethod initialize-instance :after ((obj channel-users) &key)
  (setf (potato.db:persisted-entry/couchdb-id obj)
        (channel-users-mapping-name (channel-users/channel-id obj))))

(declaim (inline ensure-channel-id))
(defun ensure-channel-id (channel)
  (etypecase channel
    (string channel)
    (channel (channel/id channel))))

(declaim (inline ensure-channel))
(defun ensure-channel (channel)
  (etypecase channel
    (string (potato.db:load-instance 'channel channel))
    (channel channel)))

(defun user-is-in-channel-p (channel user)
  "Checks if USER is a member of CHANNEL. Returns the channel-users
instance as a second value."
  (let ((users (potato.db:load-instance 'channel-users (channel-users-mapping-name (ensure-channel-id channel)))))
    (values
     (member (ensure-user-id user) (channel-users/users users) :key #'car :test #'string=)
     users)))

(defun check-user-in-channel (channel)
  (unless (user-is-in-channel-p channel (current-user))
    (error "User ~s is not a member of channel ~s" (user/id (current-user)) channel)))

(defun find-channels-for-group (group)
  (potato.db:invoke-view-and-load-instances 'channel "channel" "channels_for_group" :key (ensure-group-id group)))

;;;
;;;  User descriptions
;;;

(defun user-descriptions-for-channel-members-memcached-key (channel-id)
  (format nil "memberdescriptions-~a" channel-id))

(defun user-descriptions-for-channel-members (channel)
  "For the given CHANNEL, return a list of users in that channel in
the following form: \(ID DESCRIPTION IMAGE-NAME)"
  (let* ((channel-id (ensure-channel-id channel))
         (key (user-descriptions-for-channel-members-memcached-key channel-id))
         (cached (cl-memcached:mc-get (list key))))
    (if cached
        (potato.common:decode-conspack-with-interning (fifth (car cached)))
        ;; ELSE: Not in cache, load it and update the cache
        (let* ((channel-users (potato.db:load-instance 'channel-users (channel-users-mapping-name channel-id)))
               (user-ids (mapcar (lambda (v) (symbol-name (car v))) (channel-users/users channel-users)))
               (list (mapcar (lambda (uid)
                               ;; TODO: This could be faster by using
                               ;; the (currently commented) view
                               ;; "user_description".
                               (let ((user (potato.db:load-instance 'user uid)))
                                 (list (user/id user)
                                       (user/description user)
                                       (user/nickname user)
                                       (potato.user-image:image-url-for-user user))))
                             user-ids)))
          (cl-memcached:mc-set key (conspack:encode list))
          list))))

(defun flush-user-descriptions-for-channel-members (channel-id)
  (cl-memcached:mc-del (user-descriptions-for-channel-members-memcached-key channel-id)))

(potato.db:define-hook-fn flush-channel-member-descriptions channel-users (obj :type (:save :delete))
  (flush-user-descriptions-for-channel-members (channel-users/channel-id obj)))

(potato.db:define-hook-fn flush-channel-members-at-user-change user (obj :type (:save :delete))
  (dolist (cid (find-channels-for-user obj))
    (flush-user-descriptions-for-channel-members cid)))

;;;
;;;  User/channel management
;;;

(defgeneric add-user-to-channel (channel user)
  (:method ((channel string) user)
    (add-user-to-channel (potato.db:load-instance 'channel channel) user))
  (:method ((channel channel) user)
    (check-group-access (channel/group channel) :user user)
    (when (channel/deleted channel)
      (error "Can't add users to deleted channels"))
    (let* ((user-id (ensure-user-id user))
           (mapping (channel-users-mapping-name channel)))
      (potato.db:call-clouchdb-update-function "channel" "add_user_to_channel" mapping
                                               (list (cons "user_id" user-id)
                                                     (cons "current_date" (format-timestamp nil (local-time:now)))))
      (potato.db:flush-cache-for-object-if-enabled (find-class 'channel-users) mapping)
      (flush-cached-user-channel-state user-id)
      (flush-cached-group-channel-tree user-id)
      (flush-user-descriptions-for-channel-members (channel/id channel))
      ;; Publish a notification to rabbitmq so that clients can be notified. The format of the message
      ;; is similar to that of the :USER-NAME-CHANGE message.
      (with-pooled-rabbitmq-connection (conn)
        (cl-rabbit:basic-publish conn 1
                                 :exchange *channel-exchange-name*
                                 :routing-key (format nil "user-join.~a"
                                                      (encode-name-for-routing-key (channel/id channel)))
                                 :body (lisp-to-binary (list :user-join
                                                             (user/id user)
                                                             (potato.core:user/description user)
                                                             (potato.user-image:image-url-for-user user))))))))

(defun remove-user-from-channel (channel user)
  "Removes the user USER from CHANNEL. This function will raise an
error if an attempt is made to remove a user from a private channel.
No error is raised if the user wasn't already a member of the
channel."
  (let* ((uid (ensure-user-id user))
         (cid (ensure-channel-id channel))
         (mapping (potato.db:load-instance 'channel-users (channel-users-mapping-name cid))))
    (when (eq (channel-users/group-type mapping) :private)
      (error "Users cannot be removed from private channels. uid=~s, cid=~s" uid cid))
    (potato.db:call-clouchdb-update-function "channel" "remove_user_from_channel"
                                             (potato.db:persisted-entry/couchdb-id mapping)
                                             (list (cons "user_id" uid)))
    (potato.db:flush-cache-for-instance mapping)
    (flush-cached-user-channel-state uid)
    (flush-cached-group-channel-tree uid)))

(defun load-channel-with-check (id &key user (if-not-joined :error))
  "Load channel ID, check that the current user is allowed to access
it and return the channel instance. Also returns the channel-users
instance as a second value, unless the user is not a member of the
channel and IF-NOT-JOINED is :LOAD, in which case the second value is
NIL."
  (check-type if-not-joined (member :join :error :ignore :load))
  (let ((user (or user (current-user)))
        (channel (potato.db:load-instance 'channel id)))
    (check-group-access (channel/group channel) :user user)
    (multiple-value-bind (in-channel channel-users)
        (user-is-in-channel-p channel user)
      (if in-channel
          (values channel channel-users)
          ;; No mapping, perform action based on the passed parameter
          (ecase if-not-joined
            (:join (progn (add-user-to-channel channel user) channel))
            (:error (raise-permission-error "User is not in channel"))
            (:ignore (values nil nil))
            (:load (values channel nil)))))))

(defun load-channel-users (cid)
  (potato.db:load-instance 'channel-users (channel-users-mapping-name cid)))

(defun load-channel-users-with-check (id &key (if-not-joined :error))
  (multiple-value-bind (channel channel-users)
      (load-channel-with-check id :if-not-joined if-not-joined)
    (if channel
        (values (or channel-users
                    (load-channel-users (channel/id channel)))
                channel)
        ;; ELSE: User is not a member of channel
        (values nil nil))))

(defun create-channel (name group initial-user-ids &key topic)
  (let ((group (ensure-group group))
        channel channel-users)
    (recover-if-fail
        (progn
          (setq channel (make-instance 'channel
                                       :name name
                                       :topic (or topic "Topic has not been set")
                                       :group (group/id group)
                                       :domain (group/domain group)))
          (potato.db:save-instance channel)
          (let ((now-timestamp (format-timestamp nil (local-time:now))))
            (setq channel-users (make-instance 'channel-users
                                               :channel-id (channel/id channel)
                                               :name name
                                               :group (group/id group)
                                               :domain (group/domain group)
                                               :group-type (group/type group)
                                               :users (mapcar (lambda (user-id)
                                                                (cons (intern user-id "KEYWORD")
                                                                      `((:|count| . 0)
                                                                        (:|last_read| . ,now-timestamp))))
                                                              initial-user-ids)))
            (potato.db:save-instance channel-users)
            (flush-cached-group-channel-tree-for-domain (group/domain group)))
          channel)
      ;; Recovery forms
      (when channel
        (%recovery/delete-channel channel)))))

(defun %recovery/delete-channel (channel)
  "Delete a channel. IMPORTANT! This is an internal function that is
only to be called when recovering from a channel or domain creation
failure. If this function is called under any other circumstance, the
result will be orphaned objects in the database, and possibly crashes
when those objects are loaded.

This function is called from MAKE-AND-SAVE-DOMAIN and CREATE-CHANNEL."
  (log:warn "Deleting channel: ~a" (channel/id channel))
  (handler-case
      (clouchdb:delete-document (channel-users-mapping-name (channel/id channel)))
    (clouchdb:document-missing ()
      (log:trace "No channel-users mapping when deleting channel: ~a" (channel/id channel))))
  (potato.db:remove-instance-nofail channel))

(defun disable-channel (channel)
  "This function is the standard way of deleting channels. The users
will be removed from the channel and it will be hidden from channel
lists. However, no data is actually deleted and it can be subsequently
recovered."
  (let ((channel (ensure-channel channel)))
    ;; First, mark the channel as deleted
    (setf (channel/deleted channel) t)
    (potato.db:save-instance channel)
    ;; Then, update the channel-users instance by marking it, too, as
    ;; deleted as well as removing all the users.
    (let ((channel-users (potato.db:load-instance 'channel-users (channel-users-mapping-name (channel/id channel)))))
      (setf (channel-users/deleted channel-users) t)
      (let ((ul (channel-users/users channel-users)))
        (setf (channel-users/users channel-users) nil)
        (potato.db:save-instance channel-users)
        ;; Finally, flush the channel trees for all former members of the channel
        (dolist (u ul)
          (flush-cached-group-channel-tree (symbol-name (car u))))))))

(defun update-channel-visibility-for-user (channel user visible-p)
  (let* ((cid (ensure-channel-id channel))
         (uid (ensure-user-id user))
         (mapping-id (channel-users-mapping-name cid)))
    (potato.db:call-clouchdb-update-function "channel" "mark_hidden" mapping-id
                                             `(("user_id" . ,uid)
                                               ("show" . ,(if visible-p "1" "0"))))
    (potato.db:flush-cache-for-object-if-enabled (find-class 'channel-users) mapping-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Group channel tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-domain-tree-memcached-key (user)
  (concatenate 'string "domaintree-" (ensure-user-id user)))

(defun %domain-tree-for-user-as-template-data (user)
  (let ((domains (clouchdb:invoke-view "domain" "domains_for_user" :key (ensure-user-id user))))
    (loop
       for domain-row in (getfield :|rows| domains)
       for domain-val = (getfield :|value| domain-row)
       for domain-id = (getfield :|domain| domain-val)
       for domain = (potato.db:load-instance 'domain domain-id)
       collect `((:domain-id . ,domain-id)
                 (:domain-name . ,(domain/name domain))
                 (:domain-role . ,(getfield :|role| domain-val))
                 (:groups . ,(loop
                                with groups = (clouchdb:invoke-view "group" "groups_for_user"
                                                                    :start-key (list domain-id user nil)
                                                                    :end-key (list domain-id user 'clouchdb:json-map))
                                for group-row in (getfield :|rows| groups)
                                for group-val = (getfield :|value| group-row)
                                for group-id = (getfield :|id| group-row)
                                collect `((:group-id . ,group-id)
                                          (:group-name . ,(getfield :|group_name| group-val))
                                          (:group-type . ,(intern (getfield :|group_type| group-val) #.(find-package "KEYWORD")))
                                          (:channels . ,(loop
                                                           with channels = (clouchdb:invoke-view "channel" "channels_for_group"
                                                                                                 :key group-id)
                                                           for channel-row in (getfield :|rows| channels)
                                                           for channel-val = (getfield :|value| channel-row)
                                                           unless (getfield :|deleted| channel-val :accept-missing t)
                                                           collect `((:channel-id . ,(getfield :|id| channel-row))
                                                                     (:channel-name . ,(getfield :|name| channel-val))))))))))))

(defun domain-tree-for-user-as-template-data (user)
  (let* ((user-id (ensure-user-id user))
         (memcached-key (make-domain-tree-memcached-key user-id))
         (cached (cl-memcached:mc-get (list memcached-key))))
    (if cached
        ;; Cached result found, decode and return
        (potato.common:decode-conspack-with-interning (fifth (car cached)))
        ;; No cached result, recompute and save
        (let ((domain-tree (%domain-tree-for-user-as-template-data user-id)))
          (cl-memcached:mc-set memcached-key (conspack:encode domain-tree))
          domain-tree))))

(defun flush-cached-group-channel-tree (user)
  (cl-memcached:mc-del (make-domain-tree-memcached-key user)))

(defun flush-cached-group-channel-tree-for-domain (domain)
  (let ((result (clouchdb:invoke-view "domain" "users_in_domain" :key (ensure-domain-id domain))))
    (dolist (row (getfield :|rows| result))
      (flush-cached-group-channel-tree (getfield :|user| (getfield :|value| row))))))
