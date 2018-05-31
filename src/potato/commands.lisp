;;;
;;;  Handling of external commands. These commands are generally
;;;  issued from the main binary using the --cmd parameter.
;;;

(in-package :potato.commands)

(declaim #.potato.common::*compile-decl*)

(defun trim-spaces (s)
  (string-trim (coerce '(#\Space #\Tab) 'string) s))

(declaim (inline char-whitespace-p))
(defun char-whitespace-p (ch)
  #+sb-unicode
  (sb-unicode:whitespace-p ch)
  #-sb-unicode
  (position ch (coerce '(#\Space #\Tab) 'string)))

(defun parse-args (s)
  (labels ((clear-current-word ()
             (make-array 0 :element-type 'character :adjustable t :fill-pointer t)))
    (loop
       with state = :whitespace
       with result = nil
       with current-word = (clear-current-word)
       with pre-escape-state = nil
       for ch across s
       do (ecase state
            (:whitespace (cond
                           ((eql ch #\")
                            (setq state :quote))
                           ((eql ch #\\)
                            (setq pre-escape-state state)
                            (setq state :escape))
                           ((not (char-whitespace-p ch))
                            (vector-push-extend ch current-word)
                            (setq state :word))))
            (:quote (cond ((eql ch #\")
                           (setq state :whitespace)
                           (push current-word result)
                           (setq current-word (clear-current-word))
                           (setq state :post-quote))
                          ((eql ch #\\)
                           (setq pre-escape-state state)
                           (setq state :escape))
                          (t
                           (vector-push-extend ch current-word))))
            (:word (cond ((char-whitespace-p ch)
                          (setq state :whitespace)
                          (push current-word result)
                          (setq current-word (clear-current-word)))
                         ((eql ch #\\)
                          (setq pre-escape-state state)
                          (setq state :escape))
                         (t
                          (vector-push-extend ch current-word))))
            (:escape (progn
                       (vector-push-extend ch current-word)
                       (setq state pre-escape-state)))
            (:post-quote (cond ((char-whitespace-p ch)
                                (setq state :whitespace))
                               (t
                                (error "Non-whitespace after end quote in args: ~a" s)))))
       finally (progn
                 (cond ((eq state :word)
                        (push current-word result))
                       ((not (or (eq state :whitespace) (eq state :post-quote)))
                        (error "Unable to parse args: ~a" s)))
                 (return (reverse result))))))

(defvar *command-map* (receptacle:make-hash-map :test 'equal))

(defmacro define-command (name command required-params optional-params description long-description &body body)
  (let ((args-sym (gensym "ARGS"))
        (length-sym (gensym "LEN")))
    `(progn
       (defun ,name (,args-sym)
         (let ((,length-sym (length ,args-sym)))
           (when (< ,length-sym ,(length required-params))
             (error "Not enough parameters"))
           (when (> ,length-sym ,(+ (length required-params) (length optional-params)))
             (error "Too many parameters"))
           (let (,@(loop
                      for sym in required-params
                      for index from 0
                      collect `(,(car sym) (nth ,index ,args-sym)))
                 ,@(loop
                      for sym in optional-params
                      for index from (length required-params)
                      collect `(,(car sym) (nth ,index ,args-sym))))
             ,@body)))
       (setf (receptacle:hash-get *command-map* ,command)
             (list ',name ,command ',required-params ',optional-params ,description ,long-description)))))

(defun arg-is-active (arg &optional default-active-p)
  (cond ((null arg)
         default-active-p)
        ((equal arg "true")
         t)
        ((equal arg "false")
         nil)
        (t
         (error "Illegal boolean parameter: ~s" arg))))

(defun show-commands-list ()
  (let ((commands (sort (receptacle:hash-keys *command-map*) #'string<)))
    (dolist (command commands)
      (destructuring-bind (name command-string requred-params optional-params description long-description)
          (receptacle:hash-get *command-map* command)
        (declare (ignore name requred-params optional-params long-description))
        (format t "~a - ~a~%" command-string description)))))

(defun show-command-help (cmd)
  (let ((command (receptacle:hash-get *command-map* cmd)))
    (unless command
      (error "No such command: ~s" cmd))
    (destructuring-bind (name command-string requred-params optional-params description long-description)
        command
      (declare (ignore name))
      (format t "~a:~{ ~a~}~@[ [~{~a~^ ~}]~]~%~%"
              command-string
              (mapcar (alexandria:compose #'string-downcase #'symbol-name #'car) requred-params)
              (mapcar (alexandria:compose #'string-downcase #'symbol-name #'car) optional-params))
      (format t "~a~%" description)
      (alexandria:when-let ((arglist (append requred-params optional-params)))
        (format t "~%Parameters:~%")
        (dolist (arg arglist)
          (format t "  ~a: ~a~%" (string-downcase (symbol-name (car arg))) (cadr arg)))
        (format t "~%"))
      (format t "~a~%" long-description))))

(define-command list-domains "list-domains"
    ()
    ((type "domain type, CORPORATE, USER or ALL. Defaults to CORPORATE."))
    "List domains"
    "Lists all domains in the system"
  (let ((domains (if (equal type "ALL")
                     (potato.db:invoke-view-and-load-instances 'potato.core:domain "domain" "domain_list")
                     (potato.db:invoke-view-and-load-instances 'potato.core:domain "domain" "domain_list"
                                                               :key (or type "CORPORATE")))))
    (dolist (domain domains)
      (let ((domain-nickname (potato.core:find-domain-nickname-from-domain-id (potato.core:domain/id domain))))
        (format t "~a '~a' ~:[none~;~:*'~a'~]~%"
                (potato.core:domain/id domain)
                (potato.core:domain/name domain)
                (if domain-nickname (potato.core:domain-nickname/nickname domain-nickname)))))))

(define-command create-domain "create-domain"
    ((name "Name of the domain"))
    ((public "If true, anyone can join the domain. Defaults to false.")
     (auto-join "If true, a new user is automatically added to the domain, if allowed. Defaults to false."))
    "Create a new domain"
    "Creates a new domain with the given name"
  (let ((domain (potato.core:make-and-save-domain name :corporate
                                                  :public (arg-is-active public)
                                                  :join-default (arg-is-active auto-join))))
    (format t "Domain created: ~a~%" (potato.core:domain/id domain))))

(define-command set-domain-name "set-domain-name"
    ((domain "Domain ID")
     (name "New name of the domain"))
    ()
    "Set the name of a domain"
    "Set the name of a domain"
  (when (zerop (length name))
    (error "Domain name can't be blank"))
  (let ((domain (potato.db:load-instance 'potato.core:domain domain)))
    (setf (potato.core:domain/name domain) name)
    (potato.db:save-instance domain)
    (format t "Domain name updated~%")))

(define-command set-domain-nickname "set-domain-nickname"
    ((domain "The id of the domain")
     (nickname "The nickname to be associated with the domain, or the empty string to remove the nickname"))
    ()
    "Set the nickname for a domain"
    "Sets the nickname for the given domain. If the domain already had
a nickname, the old one will become available for a different domain."
  (potato.core:set-nickname-for-domain domain (if (equal nickname "") nil nickname)))

(define-command update-domain-join-default "update-domain-join-default"
    ((domain "The id of the domain")
     (join-default "A boolean indicating the join-default state"))
    ()
    "Update the join-default value for a domain"
    "Updates the join-default value for a given domain. If true, then
any user who registers and has access to the domain will be
automatically joined when their user is created."
  (let ((domain (potato.db:load-instance 'potato.core:domain domain)))
    (setf (potato.core:domain/join-default domain) (arg-is-active join-default))
    (potato.db:save-instance domain)))

(define-command set-domain-admin "set-domain-admin"
    ((domain "The id of the domain")
     (user "The user id of the user that should be set as admin"))
    ()
    "Gives admin rights to a domain"
    "The given user is desigated as an administator of the given
domain. This command will only work for users that are already members
of the domain."
  (let ((domain (potato.db:load-instance 'potato.core:domain domain))
        (user (potato.db:load-instance 'potato.core:user user)))
    (potato.core:update-domain-user-role domain user :admin))
  (format t "User updated~%"))

(define-command create-user "create-user"
    ((email "email address")
     (description "description")
     (password "password"))
    ((enable-api "if true, create API key")
     (activated "if true, activate the user. defaults to true"))
    "Create a new user"
    "Adds a new user to the system with the given email address and description"
  (let ((user (potato.workflow:register-user email description password
                                             (arg-is-active enable-api) (arg-is-active activated t))))
    (format t "User created: ~a~%" (potato.core:user/id user))))

(define-command update-user-name "update-user-name"
    ((user-id "user id")
     (description "new description"))
    ()
    "Update user description"
    "Set a new description for the given user"
  (let ((user (potato.db:load-instance 'potato.core:user user-id)))
    (setf (potato.core:user/description user) (trim-spaces description))
    (potato.db:save-instance user)
    (format t "User updated")))

(define-command user-list "list-users"
    ((domain "domain id"))
    ()
    "List users"
    "Lists all users in the specified domain"
  (let* ((result (clouchdb:invoke-view "domain" "users_in_domain" :key domain))
         (users (mapcar (lambda (row)
                          (let* ((d (getfield :|value| row))
                                 (uid (getfield :|user| d)))
                            (potato.db:load-instance 'potato.core:user uid)))
                        (getfield :|rows| result))))
    (dolist (user (sort users #'string< :key #'potato.core:user/description))
      (format t "~a ~a~%" (potato.core:user/id user) (potato.core:user/description user)))))

(define-command add-email-to-domain "add-email-to-domain"
    ((domain "domain id")
     (email "email domain name (i.e. foo.com)"))
    ()
    "Add an email to a domain"
    "Adds an email domain name to the given Potato domain. The email
domain is anything that comes after the @ sign in an email
address. Users with an email address that matches the registered
addresses will be allowed to join this domain."
  (let ((d (potato.db:load-instance 'potato.core:domain domain)))
    (if (find email (potato.core:domain/email-domains d) :test #'string=)
        (format t "Email already added to domain~%")
        (progn
          (push (trim-spaces email) (potato.core:domain/email-domains d))
          (potato.db:save-instance d)
          (format t "Domain updated~%")))))

(define-command remove-email-from-domain "remove-email-from-domain"
    ((domain "domain id")
     (email "email domain name (i.e. foo.com)"))
    ()
    "Remove an email from a domain"
    "Removes an email domain name from the given Potato domain."
  (let ((d (potato.db:load-instance 'potato.core:domain domain)))
    (if (find email (potato.core:domain/email-domains d) :test #'string=)
        (progn
          (setf (potato.core:domain/email-domains d)
                (remove email (potato.core:domain/email-domains d) :test #'string=))
          (potato.db:save-instance d)
          (format t "Domain updated~%"))
        (format t "Not removed because the email did not exist in the domain"))))

(define-command add-email-address-to-domain "add-email-address-to-domain"
    ((domain "domain id")
     (email-address "email address of the user to invite"))
    ()
    "Invites a user to a domain"
    "Creates an invitation to a domain for a specific email address.
This command works regardless of whether a user with the given
address has been created or not."
  (unless (potato.core:is-allowed-email-p email-address)
    (error "Illegal email address: ~a" email-address))
  (let ((domain (potato.db:load-instance 'potato.core:domain domain)))
    (potato.workflow:add-email-invitation-for-domain domain email-address)
    (format t "Email address ~a added to domain~%" email-address)))

(define-command create-channel "create-channel"
    ((domain "domain id")
     (name "name of channel"))
    ((group "group that the channel should be part of (defaults to the domain group)"))
    "Create a channel"
    "Creates a new channel"
  (let* ((d (potato.db:load-instance 'potato.core:domain domain))
         (g (if group
                (potato.db:load-instance 'potato.core:group group)
                (potato.core:find-default-group-in-domain d))))
    (unless (equal (potato.core:group/domain g) (potato.core:domain/id d))
      (error "Group is not part of the specified domain"))
    (let ((channel (potato.core:create-channel name g nil)))
      (format t "Created channel: ~a~%" (potato.core:channel/id channel)))))

(define-command list-channels "list-channels"
    ((domain "domain id"))
    ((group "group to limit the channel list for"))
    "Lists channels"
    "Shows all channels in a given domain. If given, the parameter
group limits the list of channels to that group only."
  (let* ((d (potato.db:load-instance 'potato.core:domain domain))
         (channels (if group
                       (let ((g (potato.db:load-instance 'potato.core:group group)))
                         (unless (equal (potato.core:group/domain g) (potato.core:domain/id d))
                           (error "Group is not part of the specified domain"))
                         (potato.core:find-channels-for-group g))
                       (let ((groups (potato.core:find-groups-in-domain d)))
                         (loop
                            for group in groups
                            append (potato.core:find-channels-for-group group))))))
    (dolist (channel channels)
      (let ((channel-nickname (potato.core:find-channel-nickname-from-channel-id (potato.core:channel/id channel))))
        (format t "~a '~a' ~:[none~;~:*'~a'~]~%"
                (potato.core:channel/id channel)
                (potato.core:channel/name channel)
                (if channel-nickname (potato.core:channel-nickname/nickname channel-nickname)))))))

(define-command set-channel-name "set-channel-name"
    ((channel "The id of the channel")
     (name "The new name for the channek"))
    ()
    "Set the channel name"
    "Set the channel name"
  (when (zerop (length name))
    (error "Channel name can't be blank"))
  (let ((loaded (potato.db:load-instance 'potato.core:channel channel))
        (channel-users (potato.core:load-channel-users channel)))
    (setf (potato.core:channel/name loaded) name)
    (potato.db:save-instance loaded)
    (setf (potato.core:channel-users/name channel-users) name)
    (potato.db:save-instance channel-users)
    (format t "Channel name updated~%")))

(define-command set-channel-nickname "set-channel-nickname"
    ((channel "The id of the channel")
     (nickname "The nickname to be associated with the channel, or the empty string to remove the nickname"))
    ()
    "Set the nickname for a channel"
    "Sets the nickname for the given channel. If the channel already had a nickname, the old one will become available for a different channel."
  (potato.core:set-nickname-for-channel channel (if (string= nickname "") nil nickname)))

(define-command set-channel-topic "set-channel-topic"
    ((channel "The is of the channel")
     (topic "The topic for this channel"))
    ()
    "Set the topic for a channel"
    "Set the topic for a channel."
  (let ((channel (potato.db:load-instance 'potato.core:channel channel)))
    (setf (potato.core:channel/topic channel) topic)
    (potato.db:save-instance channel)))

(define-command create-group "create-group"
    ((domain "domain id")
     (name "name of the group"))
    ()
    "Create a new group"
    "Creates a group in the given domain."
  (let ((domain (potato.db:load-instance 'potato.core:domain domain)))
    (let ((group (potato.core:create-group name domain)))
      (format t "Created group: ~a~%" (potato.core:group/id group)))))

(define-command add-user-to-group "add-user-to-group"
    ((group "group id")
     (user "user id"))
    ((role "role, defaults to USER"))
    "Add a user to a group"
    "Add a user to a group. The user must be a member of the domain that the group belongs to.
Valid values for role is: user, admin"
  (let ((user (potato.db:load-instance 'potato.core:user user))
        (group (potato.db:load-instance 'potato.core:group group)))
    (unless (potato.core:user-is-in-domain-p (potato.core:group/domain group) user)
      (error "User is not a member of domain: ~a" (potato.core:group/domain group)))
    (let ((role-id (if role (string-case:string-case ((string-downcase role))
                              ("user" potato.core:+GROUP-USER-TYPE-USER+)
                              ("admin" potato.core:+GROUP-USER-TYPE-ADMIN+)
                              (t (error "Illegal role id")))
                       potato.core:+GROUP-USER-TYPE-USER+)))
      (potato.core:add-user-to-group group user role-id)
      (format t "User ~a added to group ~a~%" (potato.core:user/id user) (potato.core:group/id group)))))

(define-command remove-user-from-group "remove-user-from-group"
    ((group "group id")
     (user "user id"))
    ()
    "Remove a user from a group"
    "Remove a user from a group"
  (let ((user (potato.db:load-instance 'potato.core:user user))
        (group (potato.db:load-instance 'potato.core:group group)))
    (potato.core:remove-user-from-group group user)
    (format t "User ~a removed from group ~a~%" (potato.core:user/id user) (potato.core:group/id group))))

(define-command user-info "user-info"
    ((user "user id"))
    ()
    "Show user details"
    "Show user details"
  (let ((user (potato.db:load-instance 'potato.core:user user)))
    (format t "ID: ~a~%" (potato.core:user/id user))
    (format t "Description: ~a~%" (potato.core:user/description user))
    (format t "Activated date: ~a~%" (potato.core:user/activated-p user))
    (format t "New login: ~:[false~;true~]~%" (potato.core:user/new-login user))
    (format t "Primary email: ~a~%" (potato.core:user/primary-email user))
    (format t "Email addresses: ~{~a~^, ~}~%" (potato.core:user/email-addresses user))))

(define-command update-password "update-password"
    ((user "user id")
     (password "password"))
    ()
    "Update user password"
    "Update the password for a user. If blank, the password will be removed."
  (let ((user (potato.db:load-instance 'potato.core:user user))
        (clear (equal password "")))
    (if clear
        (setf (potato.core:user/password user) "")
        (potato.core:user/update-password user password))
    (potato.db:save-instance user)
    (format t "Password ~:[updated~;cleared~]~%" clear)))

(define-command list-groups "list-groups"
    ((domain "domain id"))
    ()
    "List groups in a domain"
    "List groups in a domain "
  (let ((domain (potato.db:load-instance 'potato.core:domain domain)))
    (loop
      for group in (potato.core:find-groups-in-domain domain)
      do (format t "~a ~a~%" (potato.core:group/id group) (potato.core:group/name group)))))

(define-command group-info "group-info"
    ((group "group id"))
    ()
    "Show group info"
    "Show group info"
  (let ((group (potato.db:load-instance 'potato.core:group group)))
    (format t "ID: ~a~%" (potato.core:group/id group))
    (format t "Name: ~a~%" (potato.core:group/name group))
    (format t "Type: ~a~%" (symbol-name (potato.core:group/type group)))
    (format t "Email-domains: ~{~a~^, ~}~%" (potato.core:group/email-domains group))
    (format t "Members:~%  ~{~s~^, ~}~%"
            (mapcar (lambda (v)
                      (let ((user (potato.core:load-user (getfield :|user_id| v))))
                        (list (potato.core:user/id user)
                              (potato.core:user/description user)
                              (getfield :|role| v))))
                    (potato.core:group/users group)))))

(define-command set-user-group-role "set-user-group-role"
    ((user "user id")
     (group "group id")
     (role "role"))
    ()
    "Set the role for a given user in a group"
    "Set the role for a given user in a group. The role is one of: USER, ADMIN"
  (let ((user (potato.core:load-user user))
        (group (potato.db:load-instance 'potato.core:group group)))
    (unless (member (potato.core:group/type group) '(:standard :domain-default))
      (error "Can only change role for groups of type STANDARD or DOMAIN-DEFAULT"))
    (unless (member role (list potato.core:+group-user-type-user+ potato.core:+group-user-type-admin+)
                    :test #'equal)
      (error "Illegal role type"))
    (potato.core:update-role-for-group-user group user role)))

(define-command create-views "create-views"
    ()
    ()
    "Create views"
    "Create the couchdb views. Each view is removed before being added again, in order
to ensure that they are properly recreated."
  (potato.views:init-views))

(define-command test-email "test-email"
    ((email "Email address"))
    ((subject "Subject")
     (text "Text content"))
    "Send test email"
    "Send a test email to the given address."
  (potato.email:send-email (make-instance 'potato.email:mail-descriptor
                                          :to-name email
                                          :to-email email
                                          :subject (or subject "Test mail")
                                          :text-content (or text "Test mail"))))

(defun run-command (cmd)
  (multiple-value-bind (match strings)
      (cl-ppcre:scan-to-strings "^([a-zA-Z0-9-]+)(?: +(.*))?$" cmd)
    (unless match
      (error "Illegal command: ~s" cmd))
    (let* ((name (aref strings 0))
           (args-string (aref strings 1))
           (args (if args-string (parse-args args-string) nil)))
      (if (equal name "help")
          (cond ((null args)
                 (show-commands-list))
                ((null (cdr args))
                 (show-command-help (car args)))
                (t
                 (error "Usage: help [command]")))
          (let ((command (receptacle:hash-get *command-map* name)))
            (unless command
              (error "No such command: ~s" name))
            (funcall (car command) args))))))
