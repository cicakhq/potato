(in-package :potato.core)

(declaim #.potato.common::*compile-decl*)

(deftype domain-user-role () '(member :user :admin :private))
(deftype domain-type () '(member :private :corporate))

(defclass domain ()
  ((name          :type string
                  :initarg :name
                  :accessor domain/name
                  :persisted-p t)
   (domain-type   :type domain-type
                  :initarg :domain-type
                  :reader domain/domain-type
                  :persisted-p t
                  :persisted-type :symbol)
   (email-domains :type list
                  :initarg :email-domains
                  :initform nil
                  :accessor domain/email-domains
                  :persisted-p t
                  :persisted-type (:list :string))
   (join-default  :type t
                  :initarg :join-default
                  :initform nil
                  :accessor domain/join-default
                  :persisted-p t
                  :persisted-type :boolean
                  :persisted-allow-missing-value t
                  :persisted-missing-default nil)
   (join-public   :type t
                  :initarg :public
                  :initform nil
                  :accessor domain/public
                  :persisted-p t
                  :persisted-type :boolean
                  :persisted-allow-missing-value t
                  :persisted-missing-default nil))
  (:metaclass potato.db:persisted-entry-class)
  (:memcached-enabled-p t))

(defmethod print-object ((obj domain) stream)
  (print-unreadable-safely (name) obj stream
    (format stream "~s" name)))

(defgeneric domain/id (obj)
  (:method ((obj domain))
    (potato.db:persisted-entry/couchdb-id obj)))

(declaim (inline ensure-domain-id))
(defun ensure-domain-id (domain)
  (etypecase domain
    (domain (domain/id domain))
    (string domain)))

(declaim (inline ensure-domain))
(defun ensure-domain (domain)
  (etypecase domain
    (domain domain)
    (string (potato.db:load-instance 'domain domain))))

(defclass domain-user ()
  ((user   :type string
           :initarg :user
           :reader domain-user/user
           :persisted-p t)
   (domain :type string
           :initarg :domain
           :reader domain-user/domain
           :persisted-p t)
   (role   :type domain-user-role
           :initarg :role
           :accessor domain-user/role
           :persisted-p t
           :persisted-type :symbol))
  (:metaclass potato.db:persisted-entry-class)
  (:couchdb-type :|memberdomain|)
  (:memcached-enabled-p t))

(defmethod print-object ((obj domain-user) stream)
  (print-unreadable-safely (user domain role) obj stream
    (format stream "USER ~s DOMAIN ~s ROLE ~s" user domain role)))

(defun make-user-domain-mapping-id (user-id domain-id)
  (format nil "~a-~a-~a"
          "memberdomain"
          (encode-name user-id)
          (encode-name domain-id)))

(defmethod initialize-instance :after ((obj domain-user) &key)
  (unless (potato.db:persisted-entry/couchdb-id obj)
    (setf (potato.db:persisted-entry/couchdb-id obj)
          (make-user-domain-mapping-id (domain-user/user obj) (domain-user/domain obj)))))

(defclass domain-email-invitation ()
  ((domain :type string
           :initarg :domain
           :reader domain-email-invitation/domain
           :persisted-p t)
   (email  :type string
           :initarg :email
           :initform nil
           :reader domain-email-invitation/email
           :persisted-p t))
  (:metaclass potato.db:persisted-entry-class))

(defun make-domain-email-invitation-mapping-id (domain email)
  (unless (and (stringp email) (plusp (length email)))
    (error "Illegal email format"))
  (let ((domain-id (ensure-domain-id domain)))
    (format nil "domaininvitation-~a-~a" (encode-name domain-id) (encode-name email))))

(defmethod initialize-instance :after ((obj domain-email-invitation) &key)
  (with-slots (domain email) obj
    (setf (potato.db:persisted-entry/couchdb-id obj) (make-domain-email-invitation-mapping-id domain email))))

(defmethod print-object ((obj domain-email-invitation) stream)
  (print-unreadable-safely (domain email) obj stream
    (format stream "DOMAIN ~s EMAIL ~s" domain email)))

(defun find-domain-email-invitations-for-email (email)
  (let* ((result (clouchdb:invoke-view "domain" "invitations_for_email" :key email)))
    (loop
       for row in (getfield :|rows| result)
       collect (potato.db:load-instance-from-doc 'domain-email-invitation (getfield :|value| row)))))

(defun find-domain-email-invitations-for-email-addresses (emails)
  (let ((invitations (loop
                       for email in emails
                       for v = (find-domain-email-invitations-for-email email)
                       append v)))
    (remove-duplicates invitations :test #'equal :key #'domain-email-invitation/domain)))

(defun find-domain-email-invitations-for-domain (domain)
  (let ((result (potato.db:invoke-view-and-load-instances 'domain-email-invitation
                                                          "domain" "email_invitations_for_domain"
                                                          :key (ensure-domain-id domain))))
    (mapcar #'domain-email-invitation/email result)))

(defun make-and-save-domain (name type &key email-domains public join-default)
  (let (domain group channel)
    (recover-if-fail
        (progn
          (setq domain (make-instance 'potato.core:domain
                                      :name name
                                      :domain-type type
                                      :email-domains email-domains
                                      :public (if public t nil)
                                      :join-default (if join-default t nil)))
          (potato.db:save-instance domain)
          ;; Create default group
          (setq group (make-instance 'potato.core:group
                                     :domain (potato.core:domain/id domain)
                                     :name "Default group"
                                     :type :domain-default))
          (potato.db:save-instance group)
          ;; Create channel in default group.
          (setq channel (create-channel "Default channel" group nil))
          domain)
      ;; Recovery forms
      (log:warn "Recovering from failed domain creation. domain=~s, group=~s, channel=~s" domain group channel)
      (when channel (%recovery/delete-channel channel))
      (when group (potato.db:remove-instance-nofail group))
      (when domain (potato.db:remove-instance-nofail domain)))))

(defun find-domains-for-email-addresses (email-addresses)
  (loop
     for email in email-addresses
     append (multiple-value-bind (match strings)
                (cl-ppcre:scan-to-strings "^[^@]+@([^@]+)$" email)
              (unless match
                (error "Illegal email format"))
              (let ((result (clouchdb:invoke-view "domain" "domains_for_mail" :key (aref strings 0))))
                (loop
                   for row in (getfield :|rows| result)
                   collect (potato.db:load-instance-from-doc 'domain (getfield :|value| row)))))))

(defun find-public-domains ()
  (potato.db:invoke-view-and-load-instances 'domain "domain" "public_domains"))

(defun load-available-domains-for-emails (emails)
  (let ((domains (append (find-public-domains)
                         (find-domains-for-email-addresses emails)
                         (mapcar (lambda (v)
                                   (potato.db:load-instance 'domain (domain-email-invitation/domain v)))
                                 (find-domain-email-invitations-for-email-addresses emails)))))
    (remove-duplicates domains :key #'domain/id :test #'equal)))

(defgeneric load-available-domains-for-user (user)
  (:method ((user string))
    (load-available-domains-for-user (load-user user)))
  (:method ((user user))
    (load-available-domains-for-emails (user/email-addresses user))))

(defun remove-user-from-domain (domain user)
  (let* ((user (ensure-user user))
         (domain (ensure-domain domain))
         (domain-id (domain/id domain))
         (domain-user (potato.db:load-instance 'domain-user (make-user-domain-mapping-id (user/id user) domain-id))))
    ;; Remove the user from the domain's groups
    (let* ((groups-for-domain (find-groups-in-domain domain))
           (groups-for-user-domain (remove-if-not (lambda (group)
                                                    (equal (group/domain group) domain-id))
                                                  groups-for-domain)))
      (dolist (group groups-for-user-domain)
        (remove-user-from-group group user)))
    ;; Remove the user from the channels that belong to the group
    (let* ((channels-for-user (find-channels-for-user user))
           (channels-for-user-domain (remove-if-not (lambda (ch)
                                                      (equal (channel/domain ch) domain-id))
                                                    channels-for-user)))
      (dolist (ch channels-for-user-domain)
        (remove-user-from-channel ch user)))
    ;; Remove the user from the domain
    (potato.db:remove-instance domain-user)))

(defun update-user-role-in-domain (domain user role)
  (let* ((user (ensure-user user))
         (domain (ensure-domain domain))
         (domain-user (potato.db:load-instance 'domain-user
                                              (make-user-domain-mapping-id (user/id user) (domain/id domain)))))
    (unless (and (eq (domain/domain-type domain) :corporate)
                 (member role '(:user :admin)))
      (error "Illegal user role: ~s for given domain" role))
    (setf (domain-user/role domain-user) role)
    (potato.db:save-instance domain-user)))

(defun find-channels-in-domain (domain)
  (potato.db:invoke-view-and-load-instances 'potato.core:channel "channel" "channels_in_domain"
                                            :key (ensure-domain-id domain)))

(defun find-default-group-in-domain (domain)
  (let ((groups (find-groups-in-domain domain)))
    (find :domain-default groups :key #'group/type)))

(defun domain-mapping-row->list (row)
  (let ((value (getfield :|value| row)))
    (list (getfield :|domain| value)
          (getfield :|domain_name| value)
          (intern (getfield :|role| value) "KEYWORD"))))

(defun load-domains-for-user (user)
  (let* ((user-id (ensure-user-id user))
         (result (clouchdb:invoke-view "domain" "domains_for_user" :key user-id :include-docs t)))
    (mapcar (lambda (v)
              (potato.db:load-instance-from-doc 'domain-user (getfield :|doc| v)))
            (getfield :|rows| result))))

(defun load-domains-for-user%obsolete (user)
  (loop
     for domain-user in (load-domains-for-user user)
     for domain = (potato.db:load-instance 'domain (domain-user/domain domain-user))
     collect (list (domain/id domain)
                   (domain/name domain)
                   (domain-user/role domain-user))))

(defun user-is-in-domain-p (domain user)
  (let ((d (potato.db:load-instance 'domain-user
                                    (make-user-domain-mapping-id (ensure-user-id user)
                                                                 (ensure-domain-id domain))
                                    :error-if-not-found nil)))
    (when d
      (domain-user/role d))))

(defun load-private-domain-for-user (user &key (error-if-not-found t))
  (let* ((user-id (ensure-user-id user))
         (result (clouchdb:invoke-view "domain" "private_domains_for_user" :key user-id))
         (rows (getfield :|rows| result)))
    (cond ((car rows)
           (if (cdr rows)
               (error "There should only be one private domain for user ~s" user-id)
               (potato.db:load-instance 'domain (getfield :|domain| (getfield :|value| (car rows))))))
          (error-if-not-found
           (error "Private domain for user ~s is missing" user-id))
          (t
           nil))))

(defun raise-generic-domain-permission-error (user domain)
  (raise-permission-error "User ~s is not in domain ~s" (ensure-user-id user) (ensure-domain-id domain)))

(defun check-user-in-domain (domain user &key require-admin-p)
  (let ((role (user-is-in-domain-p domain user)))
    (unless role
      (raise-generic-domain-permission-error user domain))
    (when (and require-admin-p (not (member role '(:admin :private))))
      (raise-permission-error "User ~s does not have admin right in domain ~s" (ensure-user-id user) (ensure-domain-id domain))))
  (values))

(defun common-user-domains (user1 user2)
  "Returns the list of common domains which USER1 and USER2 are in"
  (let ((domains1 (mapcar #'potato.core:domain-user/domain (load-domains-for-user user1)))
        (domains2 (mapcar #'potato.core:domain-user/domain (load-domains-for-user user2))))
    (intersection domains1 domains2 :test #'equal)))

(defun load-domain-with-check (domain-id user &key require-admin-p)
  (check-type domain-id string)
  (handler-case
      (let ((domain (potato.db:load-instance 'domain domain-id)))
        (check-user-in-domain domain user :require-admin-p require-admin-p)
        domain)
    (clouchdb:document-missing () (raise-generic-domain-permission-error (current-user) domain-id))))

(defun user-count-in-domain (domain)
  "Returns the number of users in DOMAIN"
  (let ((result (clouchdb:invoke-view "domain" "user_count_in_domain" :key (ensure-domain-id domain) :reduce t)))
    (getfield :|value| (car (getfield :|rows| result)))))

(defun update-domain-user-role (domain user role)
  (check-type role domain-user-role)
  (let ((domain-user (potato.db:load-instance 'domain-user
                                              (make-user-domain-mapping-id (ensure-user-id user)
                                                                           (ensure-domain-id domain)))))
    (setf (domain-user/role domain-user) role)
    (potato.db:save-instance domain-user)))
