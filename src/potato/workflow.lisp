(in-package :potato.workflow)

(declaim #.potato.common::*compile-decl*)

(defun remove-invitation-for-email/domain (email domain)
  (check-type email string)
  (check-type domain domain)
  (clouchdb:delete-document (potato.core::make-domain-email-invitation-mapping-id domain email) :if-missing :ignore))

(defun remove-invitations-for-user/domain (user domain)
  (check-type user user)
  (check-type domain domain)
  (dolist (email (user/email-addresses user))
    (remove-invitation-for-email/domain email domain)))

(defun %add-user-to-domain (user domain role)
  (check-type user user)
  (check-type domain domain)
  (check-type role potato.core:domain-user-role)
  ;; Before adding the user to the domain, delete any outstanding invitations
  (remove-invitations-for-user/domain user domain)
  ;;(clouchdb:delete-document (potato.core::make-domain-user-invitation-mapping-id domain user) :if-missing :ignore)
  ;; Create the user to domain mapping document
  (let ((domain-user (make-instance 'domain-user
                                    :user (user/id user)
                                    :domain (domain/id domain)
                                    :role role)))
    (potato.db:save-instance domain-user)
    ;; A user automatically becomes a member of the domain-default group
    ;; when added to a domain.
    (let ((default-group (potato.core:find-default-group-in-domain domain)))
      (potato.core::add-user-to-group default-group user (if (eq role :private)
                                                             potato.core::+GROUP-USER-TYPE-ADMIN+
                                                             potato.core::+GROUP-USER-TYPE-USER+)))
    domain-user))

(defgeneric add-user-to-domain (user domain role)
  (:method ((user string) domain role)
    (add-user-to-domain (load-user user) domain role))
  (:method (user (domain string) role)
    (add-user-to-domain user (potato.db:load-instance 'domain domain) role))
  (:method ((user user) (domain domain) role)
    (%add-user-to-domain user domain role)))

(defun add-user-to-domain-with-check (domain user)
  ;; There are currently two different ways a user can join a domain:
  ;; Either the user is allowed based on the email domain, or there is
  ;; an outstanding invitation. We need to check for both cases.
  (let ((domain (potato.core:ensure-domain domain))
        (user (potato.core:ensure-user user)))
    ;; If the user is already a member of the domain, raise an error.
    (when (member (domain/id domain) (potato.core:load-domains-for-user%obsolete user) :key #'first :test #'equal)
      (raise-permission-error "User is already a member of domain"))
    (let ((available (potato.core:load-available-domains-for-user user)))
      (unless (member (domain/id domain) available :key #'domain/id :test #'equal)
        (raise-permission-error "User is not allowed to join domain"))
      (add-user-to-domain user domain :user))))

(defun create-private-domain-for-user (user)
  "Create the private domain for a user. Warning, the concept of
private domains is somewhat ill-defined, and it's probably best if
it's eliminated altogether."
  (let ((user (potato.core:ensure-user user))
        (domain (potato.core::load-private-domain-for-user user :error-if-not-found nil)))
    (when domain
      (error "User already has a private domain"))
    (let ((domain (potato.core:make-and-save-domain (format nil "Private domain for user ~a" (user/id user)) :private
                                                    :public nil :join-default nil)))
      (let ((domain-user (make-instance 'potato.core:domain-user
                                        :domain (potato.core:domain/id domain)
                                        :user (potato.core:user/id user)
                                        :role :private)))
        (potato.db:save-instance domain-user)
        domain))))

(defun register-user (email description password enable-api activated-p)
  ;; If a registration validator function is registered, call it to check if the user is allowed
  (when potato:*user-registration-validation-function*
    (let ((validator-result (funcall *user-registration-validation-function* email description)))
      (when validator-result
        (potato.core:raise-permission-error validator-result))))
  ;; For dedicated-domain installations, only allow registration if the user has access to at least one domain
  (let ((available-domains (load-available-domains-for-emails (list email))))
    (unless (or *allow-registration-without-domain*
                available-domains)
      (raise-permission-error "User does not have access to any domains"))
    ;; The user is allowed to be registered, go ahead and create it in the database
    (let ((user (potato.core::make-unregistered-user description password)))
      (when enable-api
        (potato.core::generate-and-modify-api-token user))
      (when activated-p
        (setf (potato.core:user/activated-p user) (format-timestamp nil (local-time:now))))
      ;; Before saving the user object, create the email/user mapping object. This will fail
      ;; if the email address has already been registered
      (let ((emailuser (make-instance 'potato.core:user-email
                                      :user (potato.core:user/id user)
                                      :email email)))
        (potato.db:save-instance emailuser))
      ;; Now that the email mapping object has been saved, we can save the user object
      (potato.core:save-user user)
      ;; Create the user's private domain
      (let ((domain (potato.core::make-and-save-domain (format nil "Private domain for user ~a" email) :private)))
        (add-user-to-domain user domain :private))
      ;; Add the user to all domains that the user has access to for which join-default is true
      (dolist (domain available-domains)
        (when (domain/join-default domain)
          (if (eq (domain/domain-type domain) :corporate)
              (add-user-to-domain user domain :user)
              ;; ELSE: This indicates a violation of an invariant, only :CORPORATE domains should be marked join-default
              (log:error "Domain ~s is marked as auto-join but is not of type :CORPORATE" (domain/id domain)))))
      user)))

(defun add-email-invitation-for-domain (domain email)
  (check-type domain domain)
  (log:trace "Creating invite for domain: ~s, user: ~s" (potato.core:domain/id domain) email)
  (let ((user (potato.core:load-user-by-email email :error-if-not-found nil)))
    (if (and user (potato.core:user-is-in-domain-p domain user))
        (st-json:jso "result" "error"
                     "details" "User is already in domain")
        ;; ELSE: User is not already in the domain, create the invitation
        (let ((invitation (make-instance 'potato.core:domain-email-invitation
                                         :domain (potato.core:domain/id domain)
                                         :email email)))
          (potato.db:save-instance invitation)))))

(defun remove-email-invitation-for-domain (domain email)
  (check-type domain domain)
  (remove-invitation-for-email/domain email domain))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Sending messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-usernames-and-insert-updated-reference (user-id stream)
  (let ((desc (potato.core:find-descriptions-for-users (list user-id))))
    (let ((ch (code-char #xf0001)))
      (format stream "~cuser:~a:~a~c" ch user-id (or (car desc) "") ch))))

(defun update-username-references-in-text (text)
  (with-output-to-string (s)
    (loop
       with length = (length text)
       with start = 0
       while (< start length)
       do (multiple-value-bind (match-start match-end reg-starts reg-ends)
              (cl-ppcre:scan #.(let ((ch (code-char #xf0001)))
                                 (format nil "~cuser:([a-zA-Z0-9@.-]+)(?:[^~a~a~ch]*)?~c" ch #\Newline #\Return ch ch))
                             text
                             :start start)
            (if match-start
                (progn
                  (when (> match-start start)
                    (write-string text s :start start :end match-start))
                  (setq start match-end)
                  (let ((user-id (subseq text (aref reg-starts 0) (aref reg-ends 0))))
                    (find-usernames-and-insert-updated-reference user-id s)))
                ;; ELSE: End of matches, write the final part
                (progn
                  (write-string text s :start start)
                  (setq start length)))))))


(defun send-message-to-channel (user channel text
                                &key extra-html (send-typing-end t) (send-update t))
  "Send a message from USER to CHANNEL with content TEXT. Returns the message instance."
  (check-type user user)
  (check-type channel channel)
  (log:trace "Sending message from user: ~s on channel: ~s, text: ~s" user channel text)
  (let* ((text (ensure-printable-string text))
         (processed-text (update-username-references-in-text text))
         (message (potato.core:make-message channel user processed-text :extra-html extra-html))
         (result (potato.core:save-message message)))
    (when send-typing-end
      (potato.core:send-typing-end-notification-to-state-server channel user))
    (when send-update
      (run-future
        (with-pooled-rabbitmq-connection (conn)
          (cl-rabbit:basic-publish conn 1
                                   :exchange *message-send-exchange-name*
                                   :routing-key (format nil "~a.~a.~a"
                                                        (channel/domain channel)
                                                        (channel/id channel)
                                                        (encode-name-for-routing-key (message/from message)))
                                   :properties (list (cons :correlation-id (message/id message)))
                                   :body (babel:string-to-octets (st-json:write-json-to-string (message-cd->json message :text user)) :encoding :utf-8)))))
    result))

(defun create-channel-with-check (user group-id channel-name topic)
  (potato.core:check-group-access group-id :require-admin-p t)
  (let ((name-fixed (trim-string channel-name)))
    (unless (plusp (length name-fixed))
      (error "Channel name can't be blank"))
    (let* ((channel (create-channel name-fixed group-id (list (user/id user)) :topic topic))
           (message (make-message channel user (format nil "~a created channel" (user/description user)))))
      (save-message message)
      channel)))
