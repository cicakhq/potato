(in-package :potato.core)

(declaim #.potato.common::*compile-decl*)

(defclass user (potato.db:persisted-entry)
  ((description           :type string
                          :initarg :description
                          :accessor user/description
                          :persisted-p t
                          :record-changes-p t
                          :documentation "The full name of the user")
   (nickname              :type string
                          :initarg :nickname
                          :accessor user/nickname
                          :persisted-p t
                          :record-changes-p t
                          :documentation "Unique nickname for the user")
   (password              :type (or null string)
                          :initarg :password
                          :accessor user/password
                          :persisted-p t
                          :documentation "Hash of the user's password")
   (api-token             :type (or null string)
                          :initarg :api-token
                          :initform nil
                          :accessor user/api-token
                          :persisted-p t
                          :documentation "API token, or NIL if not set")
   (activated-p           :type (or null string)
                          :initarg :activated-p
                          :initform nil
                          :accessor user/activated-p
                          :persisted-p t
                          :documentation "If activated, this value holds the activation timestamp")
   (activate-code         :type string
                          :initarg :activate-code
                          :initform ""
                          :accessor user/activate-code
                          :persisted-p t
                          :documentation "If the user is not activated, this value holds the activation code")
   (image-name            :type string
                          :initarg :image-name
                          :initform ""
                          :accessor user/image-name
                          :persisted-p t
                          :record-changes-p t
                          :documentation "The name of the user's image. This is changed to a random when the image is changed.")
   (default-image-name    :type string
                          :initarg :default-image-name
                          :initform "1.png"
                          :accessor user/default-image-name
                          :persisted-p t
                          :persisted-allow-missing-value t
                          :persisted-missing-default "1.png"
                          :documentation "The image to use if IMAGE-NAME is not set")
   (new-login             :type t
                          :initform t
                          :initarg :new-login
                          :accessor user/new-login
                          :persisted-p t
                          :persisted-type :boolean
                          :persisted-allow-missing-value t
                          :persisted-missing-default nil
                          :documentation "If true, the user needs to be shown the initial welcome screen.")
   (note                  :type list
                          :initarg :node
                          :initform nil
                          :accessor user/note
                          :persisted-p t
                          :persisted-allow-missing-value t
                          :persisted-missing-default nil
                          :persisted-type (:alist :string)
                          :documentation "Alist of additional information attached to the user"))
  (:metaclass potato.db:persisted-entry-class)
  (:attachments-p t)
  (:memcached-enabled-p t)
  (:documentation "Instance of a user in the system"))

(defmethod print-object ((obj user) stream)
  (print-unreadable-safely (description) obj stream
    (let ((couchdb-id (if (slot-boundp obj 'potato.db::couchdb-id)
                          (slot-value obj 'potato.db::couchdb-id)
                          :not-bound)))
      (format stream "~s ~s" couchdb-id description))))

(declaim (inline ensure-user-id))
(defun ensure-user-id (user)
  (etypecase user
    (string (when (not (string-start-match "user-" user))
              (error "Unexpected user name format"))
            user)
    (user (user/id user))))

(declaim (inline ensure-user))
(defun ensure-user (user)
  (etypecase user
    (string (load-user user))
    (user user)))

(defgeneric user/id (user)
  (:method ((user user))
    (potato.db:persisted-entry/couchdb-id user)))

(defclass user-email ()
  ((user  :type string
          :initarg :user
          :reader user-email/user
          :persisted-p t)
   (email :type string
          :initarg :email
          :reader user-email/email
          :persisted-p t))
  (:metaclass potato.db:persisted-entry-class)
  (:documentation "Mapping table for user email addresses"))

(defun make-user-email-key (email)
  (concatenate 'string "useremail-" (encode-name email)))

(defmethod initialize-instance :after ((obj user-email) &key)
  (setf (potato.db:persisted-entry/couchdb-id obj)
        (make-user-email-key (user-email/email obj))))

(defmethod print-object ((obj user-email) stream)
  (print-unreadable-safely (user email) obj stream
    (format stream "USER ~s EMAIL ~s" user email)))

(defun load-user-email-by-email (email)
  (potato.db:load-instance 'user-email (make-user-email-key email)))

(defun user/email-addresses (user)
  (let* ((user-id (ensure-user-id user))
         (result (clouchdb:invoke-view "user" "emails_by_user" :key user-id)))
    (mapcar (lambda (v) (getfield :|value| v)) (getfield :|rows| result))))

(defgeneric user/primary-email (user)
  (:method ((user user))
    (let ((email-addresses (user/email-addresses user)))
      (unless email-addresses
        (error "User does not have an email address assigned: ~s" (user/id user)))
      (when (cdr email-addresses)
        (error "More than one email address for a user is not currently supported"))
      (car email-addresses))))

;;;
;;;  User nicknames
;;;

(defclass user-nickname ()
  ((nickname :type string
             :initarg :nickname
             :reader user-nickname/nickname
             :persisted-p t)
   (user     :type string
             :initarg :user
             :reader user-nickname/user
             :persisted-p t))
  (:metaclass potato.db:persisted-entry-class)
  (:documentation "Records the registered nickname for a user in order to ensure they are unique"))

(defun make-user-nickname-id (nickname)
  (unless (valid-user-nickname-p nickname)
    (error "Invalid nickname format"))
  (concatenate 'string "usernickname-" (encode-name (string-downcase nickname))))

(defmethod initialize-instance :after ((obj user-nickname) &key)
  (setf (potato.db:persisted-entry/couchdb-id obj)
        (make-user-nickname-id (user-nickname/nickname obj))))

(potato.db:define-hook-fn ensure-nickname-unique user (user :type :pre-save)
  "Hook function that creates the user-nickname instance before the user is saved."
  (multiple-value-bind (updated-p value)
      (potato.db:persisted-entry-is-value-updated user 'nickname)
    (when (and updated-p (not (equal (user/nickname user) value)))
      (let ((nick (make-instance 'user-nickname :user (user/id user) :nickname (user/nickname user))))
        ;; This call will throw an error if the nickname already exists
        (log:trace "Saving nick: ~s" (user/nickname user))
        (potato.db:save-instance nick)))))

(potato.db:define-hook-fn remove-old-nickname-if-changed user (user :type :save)
  "Hook function that removes the old user-nickname object after a user was saved."
  (multiple-value-bind (updated-p value)
      (potato.db:persisted-entry-is-value-updated user 'nickname)
    (when (and updated-p (not (equal (user/nickname user) value)))
      (log:trace "Removing nick: ~s" value)
      (let ((nick (potato.db:load-instance 'user-nickname (make-user-nickname-id value))))
        (potato.db:remove-instance nick)))))

(defun nickname-is-in-use-p (nickname)
  (if (potato.db:load-instance 'user-nickname (make-user-nickname-id nickname) :error-if-not-found nil) t nil))

(defun valid-user-nickname-p (nickname)
  (cl-ppcre:scan "^[a-zA-Z0-9_-]+$" nickname))

;;;
;;;  Passwords
;;;

(defun user/match-password (user password)
  (check-type user user)
  (let ((hashed-password (user/password user)))
    (and (plusp (length hashed-password))
         (ironclad:pbkdf2-check-password (trivial-utf-8:string-to-utf-8-bytes password) hashed-password))))

(defun user/update-password (user password)
  (check-type user user)
  (check-type password string)
  (setf (user/password user)
        (ironclad:pbkdf2-hash-password-to-combined-string (trivial-utf-8:string-to-utf-8-bytes password)
                                                          :salt (secure-random:bytes 64 secure-random:*generator*)
                                                          :iterations 20000)))

(defun save-user (user)
  (check-type user user)
  (potato.db:save-instance user))

(defun email->user-id (email)
  (concatenate 'string "user-" (string-downcase (encode-name email))))

(defun load-user (id &key (error-if-not-found t))
  (potato.db:load-instance 'user id :error-if-not-found error-if-not-found))

(defun load-user-by-email (email &key (error-if-not-found t))
  (let* ((result (clouchdb:invoke-view "user" "users_by_email" :key email))
         (rows (getfield :|rows| result)))
    (if (null rows)
        (if error-if-not-found
            (error "No user with email: ~s" email)
            ;; ELSE: No user found and error was not requested, simply return nil
            nil)
        ;; ELSE: Have results, check if there was more than one result
        (if (null (cdr rows))
            (potato.db:load-instance 'user (getfield :|value| (car rows)))
            ;; ELSE: More than one user with the same email address
            (error "More than one user with email: ~s" email)))))

(defun find-channels-for-user (user)
  (let ((uid (ensure-user-id user)))
    (let ((result (clouchdb:invoke-view "user" "channels_by_domain_and_user"
                                        :start-key (list uid nil)
                                        :end-key (list uid 'clouchdb:json-map))))
      (mapcar (lambda (v) (car (getfield :|value| v))) (getfield :|rows| result)))))

(defun generate-and-modify-api-token (user)
  (check-type user user)
  (let ((token (concatenate 'string
                            (encode-name (user/id user))
                            "-"
                            (ironclad:byte-array-to-hex-string (secure-random:bytes 32 secure-random:*generator*)))))
    (setf (user/api-token user) token)
    token))

(defparameter *user-description-memcached-prefix* "userdescription-")

(defun find-descriptions-for-users (user-ids)
  "Return a list of user full names for the corresponding user ids"
  (if user-ids
      (let* ((keys (mapcar #'(lambda (v) (concatenate 'string *user-description-memcached-prefix* v)) user-ids))
             (cached (cl-memcached:mc-get keys)))
        (loop
           for user-id in user-ids
           for key in keys
           for cached-entry = (find key cached :key #'first :test #'equal)
           if cached-entry
           collect (babel:octets-to-string (fifth cached-entry) :encoding :utf-8)
           else
           collect (handler-case
                       (let* ((user (potato.db:load-instance 'user user-id))
                              (user-name (user/description user)))
                         (cl-memcached:mc-set key user-name)
                         user-name)
                     (clouchdb:document-missing () nil))))
      ;; ELSE: User id list is empty, simply return nil
      nil))

(potato.db:define-hook-fn flush-user-descriptions user (user :type (:save :delete))
  (cl-memcached:mc-del (concatenate 'string *user-description-memcached-prefix* (user/id user))))

;;;
;;;  Password reset
;;;

(defun clear-user-password (user)
  (check-type user user)
  (setf (user/password user) "")
  (potato.db:save-instance user))
