(in-package :potato.core)

(declaim #.potato.common::*compile-decl*)

(alexandria:define-constant +GROUP-USER-TYPE-ADMIN+ "ADMIN" :test #'equal)
(alexandria:define-constant +GROUP-USER-TYPE-USER+ "USER" :test #'equal)
(alexandria:define-constant +GROUP-USER-TYPE-PRIVATE+ "PRIVATE" :test #'equal)

(deftype group-type () '(member :standard :domain-default :private))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; group
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass group ()
  ((name               :type string
                       :initarg :name
                       :accessor group/name
                       :persisted-p t)
   (domain             :type string
                       :initarg :domain
                       :reader group/domain
                       :persisted-p t)
   (users              :type t
                       :initarg :users
                       :initform nil
                       :accessor group/users
                       :persisted-p t
                       :persisted-type :json)
   (group-type         :type (or null group-type) ; TODO: Remove NULL type, only for compatibility
                       :initarg :type
                       :initform :standard
                       :reader group/type
                       :persisted-p t
                       :persisted-type :symbol
                       :persisted-allow-missing-value t)
   (authorised-domains :type list
                       :initarg :email-domains
                       :initform nil
                       :accessor group/email-domains
                       :persisted-p t
                       :persisted-type (:list :string)
                       :persisted-allow-missing-value t))
  (:metaclass potato.db:persisted-entry-class)
  (:memcached-enabled-p t))

(defmethod group/type :around ((obj group))
  "Workaround to support old-format groups without a type"
  (let ((type (call-next-method)))
    (or type :standard)))

(defmethod print-object ((obj group) stream)
  (print-unreadable-safely (name) obj stream
    (format stream "~s" name)))

(defgeneric group/id (obj))
(defmethod group/id ((obj group))
  (potato.db:persisted-entry/couchdb-id obj))

(defun ensure-group-id (group)
  (etypecase group
    (group (group/id group))
    (string group)))

(defun ensure-group (group)
  (etypecase group
    (group group)
    (string (potato.db:load-instance 'group group))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Group management support functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-groups-in-domain (domain)
  (mapcar #'(lambda (v)
              (potato.db:load-instance 'group (getfield :|id| v)))
          (getfield :|rows| (clouchdb:invoke-view "group" "groups_in_domain" :key (ensure-domain-id domain)))))

(defgeneric create-group (name domain &key users)
  (:method (name (domain string) &key users)
    (create-group name (potato.db:load-instance 'potato.core:domain domain) :users users))
  (:method ((name string) (domain domain) &key users)
    (let* ((uids (loop
                    for u in users
                    unless (user-is-in-domain-p (domain/id domain) u)
                    do (error "User ~s can't be added to the new group since it's not a member of domain ~s"
                              (ensure-user-id u) (domain/id domain))
                    collect (ensure-user-id u)))
           (group (make-instance 'group
                                :domain (domain/id domain)
                                :name name
                                :users (mapcar (lambda (v)
                                                 `((:|user_id| . ,v)
                                                   (:|role| . +GROUP-USER-TYPE-USER+)))
                                               uids))))
      (potato.db:save-instance group)
      (mapc #'flush-cached-group-channel-tree uids)
      group)))

(defgeneric add-user-to-group (group user role)
  (:method ((group string) user role)
    (add-user-to-group (potato.db:load-instance 'group group) user role))
  (:method ((group group) user role)
    (let* ((user-id (ensure-user-id user))
           (field `((:|user_id| . ,user-id)
                    (:|role|    . ,role))))
      (pushnew field (group/users group)
               :test (lambda (o1 o2) (equal (getfield :|user_id| o1) (getfield :|user_id| o2))))
      (potato.db:save-instance group)
      (flush-cached-group-channel-tree user)
      (flush-cached-user-role-for-group group user))))

(defgeneric remove-user-from-group (group user)
  (:method ((group string) user)
    (remove-user-from-group (potato.db:load-instance 'group group) user))
  (:method ((group group) user)
    (let ((uid (ensure-user-id user)))
      (setf (group/users group)
            (remove uid (group/users group) :key (lambda (v) (getfield :|user_id| v)) :test #'equal))
      (potato.db:save-instance group)
      (flush-cached-group-channel-tree user)
      (flush-cached-user-role-for-group group user))))

(defgeneric update-role-for-group-user (group user role)
  (:method ((group string) user role)
    (update-role-for-group-user (potato.db:load-instance 'group group) user role))
  (:method ((group group) user role)
    (let ((uid (ensure-user-id user)))
      (unless (member role (list +GROUP-USER-TYPE-USER+ +GROUP-USER-TYPE-ADMIN+) :test #'equal)
        (error "Illegal role specification: ~s" role))
      (let ((e (find uid (group/users group) :key (lambda (v) (getfield :|user_id| v)) :test #'equal)))
        (unless e
          (error "User ~s is not a member of group ~s" uid (group/id group)))
        (setf (getfield :|role| e) role)
        (potato.db:save-instance group))
      (flush-cached-group-channel-tree user)
      (flush-cached-user-role-for-group group uid))))

(defun groups-for-user-as-template-data (user)
  (declare (ignore user))
  ;; TODO: Remove this function
  (error "function has been disabled")
  #+nil(let ((result (clouchdb:invoke-view "group" "open_groups_for_user" :key (ensure-user-id user))))
         (loop
            for row in (getfield :|rows| result)
            for value = (getfield :|value| row)
            collect (list (cons :id (getfield :|group| value))
                          (cons :name (getfield :|group_name| value))
                          (cons :role (getfield :|role| value))))))

(defun make-user-role-for-group-memcached-key (group user)
  (format nil "user-role-group-~a-~a" (encode-name (ensure-group-id group)) (encode-name (ensure-user-id user))))

(defun flush-cached-user-role-for-group (group user)
  (cl-memcached:mc-del (make-user-role-for-group-memcached-key group user)))

(defgeneric user-role-for-group (group user)
  (:method ((group string) user)
    (let* ((uid (ensure-user-id user))
           (key (make-user-role-for-group-memcached-key group uid))
           (cached (cl-memcached:mc-get (list key))))
      (if cached
          (binary-to-lisp (fifth (car cached)))
          ;; ELSE: Not in cache. Lookup and update cache.
          (let* ((result (clouchdb:invoke-view "group" "groups_and_users"
                                               :include-docs nil
                                               :key (list group uid)))
                 (rows (getfield :|rows| result)))
            (let ((role (if (and rows (null (cdr rows)))
                            (getfield :|value| (car rows))
                            nil)))
              (cl-memcached:mc-set key (lisp-to-binary role))
              role)))))
  (:method ((group group) user)
    (let* ((uid (ensure-user-id user))
           (u (find uid (group/users group)
                    :key #'(lambda (v) (getfield :|user_id| v))
                    :test #'equal)))
      (and u (getfield :|role| u)))))

(defun user-is-admin-in-group-p (group user)
  (equal (user-role-for-group group user) +GROUP-USER-TYPE-ADMIN+))

(defun name-for-private-channel-counterpart (group user)
  (let ((uid (ensure-user-id user))
        (group (ensure-group group)))
    (unless (eq (group/type group) :private)
      (error "Group is not private: ~s" (group/id group)))
    (let ((users (group/users group)))
      (unless (= (length users) 2)
        (error "Private group does not have 2 users: ~s" (group/id group)))
      (let* ((u1 (getfield :|user_id| (first users)))
             (u2 (getfield :|user_id| (second users)))
             (counterpart-id (cond ((equal u1 uid)
                                    u2)
                                   ((equal u2 uid)
                                    u1)
                                   (t
                                    (error "User is not a member of group. user=~s, group=~s" uid (group/id group))))))
        (car (find-descriptions-for-users (list counterpart-id)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Web functions for group management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-group-access (group &key user require-admin-p)
  (let ((role (user-role-for-group group (or user (current-user)))))
    (when (or (null role)
              (and require-admin-p
                   (not (equal role +GROUP-USER-TYPE-ADMIN+))))
      (error 'permission-error :message "User does not have access to group"))))

(defun load-group-with-check (group-id &key require-admin-p)
  (let* ((group (potato.db:load-instance 'group group-id))
         (group-entry (find (user/id (current-user)) (group/users group)
                            :key #'(lambda (v) (getfield :|user_id| v))
                            :test #'equal)))
    (when (or (null group-entry)
              (and require-admin-p
                   (not (equal (getfield :|role| group-entry) +GROUP-USER-TYPE-ADMIN+))))
      (error 'permission-error :message "User is not a member of group"))
    group))
