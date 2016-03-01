(in-package :potato.api)

(declaim #.potato.common::*compile-decl*)

(alexandria:define-constant +api-url-prefix+ "/api/1.0" :test #'equal)

(potato.core:define-handler-fn-login (request-api-token-screen "/request_api_token" nil ())
  (potato.core:with-authenticated-user ()
    (let ((user (potato.core:current-user)))
      (potato.core::generate-and-modify-api-token user)
      (potato.core:save-user user))
    (hunchentoot:redirect "/settings")))

(define-condition api-error (error)
  ((message      :type string
                 :initarg :message
                 :initform "Unknown error"
                 :reader api-error/message)
   (code         :type integer
                 :initarg :code
                 :initform 500
                 :reader api-error/code)
   (extra-fields :type list
                 :initarg :extra-fields
                 :initform nil
                 :reader api-error/extra-fields))
  (:report (lambda (condition stream)
             (format stream "API error: ~a. Code: ~a" (api-error/message condition) (api-error/code condition)))))

(defmethod print-object ((obj api-error) stream)
  (print-unreadable-safely (message code) obj stream
    (format stream "MESSAGE ~s CODE ~a" message code)))

(defun raise-api-error (message &optional (code 500) extra-fields)
  (log:trace "Raising API error. message=~s, code=~s, extra-fields=~s" message code extra-fields)
  (error 'api-error :message message :code code :extra-fields extra-fields))

(defun load-user-from-api-token (req-token)
  (unless req-token
    (raise-api-error "No API token specified" hunchentoot:+http-authorization-required+))
  (let ((parts (split-sequence:split-sequence #\- req-token)))
    (unless (= (length parts) 2)
      (raise-api-error "Illegal API token format" hunchentoot:+http-forbidden+))
    (let ((user (potato.core:load-user (decode-name (first parts)) :error-if-not-found nil)))
      (cond ((not (and user
                       (potato.core:user/api-token user)
                       (string= (potato.core:user/api-token user) req-token)))
             (raise-api-error "Access denied" hunchentoot:+http-forbidden+))
            ((not (potato.core:user/activated-p user))
             (raise-api-error "User not actvated" hunchentoot:+http-forbidden+ (list "details" "not_activated"))))
      user)))

(defun verify-api-token-and-run (url fn)
  (handler-case
      (let ((potato.core::*current-auth-user* (load-user-from-api-token (hunchentoot:header-in* "api-token"))))
        (funcall fn))
    ;; Error handlers
    (api-error (condition)
      (log:debug "API error when calling ~a: ~a" url condition)
      (setf (hunchentoot:return-code*) (api-error/code condition))
      (apply #'st-json:jso
             "error" :true
             "message" (api-error/message condition)
             (api-error/extra-fields condition)))
    (potato.core:permission-error (condition)
      (log:debug "Permission error when calling ~a: ~a" url condition)
      (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+)
      (st-json:jso "error" :true
                   "error_type" "permission"
                   "message" (potato.core:potato-error/message condition)))))

(lofn:define-handler-fn (login-api-key-screesn "/login_api_key" nil ())
  (lofn:with-parameters ((key "api-key")
                         (redirect "redirect"))
    (let ((user (load-user-from-api-token key)))
      (potato.core::update-user-session user)
      (hunchentoot:redirect (or redirect "/")))))

(defmacro api-case-method (&body cases)
  (destructuring-bind (new-cases has-default-p)
      (loop
         with found = nil
         for c in cases
         unless (and (listp c)
                     (>= (length c) 2))
         do (error "Incorrectly formatted clause: ~s" c)
         when (eq (car c) t)
         do (setq found t)
         collect c into result-list
         finally (return (list result-list found)))
    (let ((method-sym (gensym)))
      `(let ((,method-sym (hunchentoot:request-method*)))
         (case ,method-sym
           ,@new-cases
           ,@(unless has-default-p
                     (list `(t (raise-api-error (format nil "Illegal request method: ~a"
                                                        (symbol-name ,method-sym))
                                                hunchentoot:+http-method-not-allowed+)))))))))

(defmacro define-api-method ((name url regexp (&rest bind-vars) &key (result-as-json t)) &body body)
  (let ((result-sym (gensym "RESULT-")))
    `(lofn:define-handler-fn (,name ,(concatenate 'string +api-url-prefix+ url) ,regexp ,bind-vars)
       (log:trace "Call to API method ~s, URL: ~s" ',name (hunchentoot:request-uri*))
       ,(if result-as-json
            `(let ((,result-sym (verify-api-token-and-run ',name (lambda () ,@body))))
               (lofn:with-hunchentoot-stream (out "application/json")
                 (st-json:write-json ,result-sym out)
                 ;; Write a final newline to make the output a bit easier to
                 ;; read when using tools such as curl
                 (format out "~c" #\Newline)))
            ;; ELSE: Don't process the result
            `(verify-api-token-and-run ',name (lambda () ,@body))))))

#+nil(defun api-get-channels-for-group (group-id &optional show-subscriptions-p subscribed-channels)
  (let ((result (clouchdb:invoke-view "channel" "channels_for_group" :key group-id)))
    (mapcar #'(lambda (v)
                (let ((id (getfield :|id| v)))
                  (apply #'st-json:jso
                         "id" id
                         "name" (getfield :|name| (getfield :|value| v))
                         (if show-subscriptions-p (list "joined" (if (member id subscribed-channels :test #'equal)
                                                                     "true"
                                                                     "false"))))))
            (getfield :|rows| result))))

(defun api-load-channels-for-group (group)
  (let ((is-private-p (eq (potato.core:group/type group) :private)))
    (loop
      for channel in (potato.core:find-channels-for-group group)
      unless (potato.core:channel/deleted channel)
        collect (st-json:jso "id" (potato.core:channel/id channel)
                             "name" (if is-private-p
                                        (potato.core:name-for-private-channel-counterpart group (potato.core:current-user))
                                        (potato.core:channel/name channel))
                             "private" (st-json:as-json-bool is-private-p)))))


(defun api-load-domain-info (domain-id include-groups-p include-channels-p)
  (let* ((domain (potato.core:load-domain-with-check domain-id (potato.core:current-user)))
         (id (potato.core:domain/id domain)))
    (apply #'st-json:jso
           "id" id
           "name" (potato.core:domain/name domain)
           "type" (symbol-name (potato.core:domain/domain-type domain))
           (if (or include-groups-p include-channels-p)
               (list "groups" (mapcar (lambda (group)
                                        (apply #'st-json:jso
                                               "id" (potato.core:group/id group)
                                               "name" (potato.core:group/name group)
                                               "type" (symbol-name (potato.core:group/type group))
                                               (if (and include-channels-p
                                                        (potato.core:user-role-for-group group (potato.core:current-user)))
                                                   (list "channels" (api-load-channels-for-group group)))))
                                      (potato.core:find-groups-in-domain id)))))))

(defun parse-and-check-input-as-json ()
  (let ((json-text (hunchentoot:raw-post-data :force-text t)))
    (when (null json-text)
      (raise-api-error "Empty input" hunchentoot:+http-not-acceptable+))
    (let ((data (st-json:read-json-from-string json-text)))
      data)))

(defun check-message-length (length)
  (when (> length potato.core:*max-message-size*)
    (raise-api-error "Message is too large" hunchentoot:+http-request-entity-too-large+)))

(define-api-method (api-version-screen "/version" nil ())
  (api-case-method
    (:get (st-json:jso "version" "1"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Domain API calls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-api-method (api-joined-domains-screen "/domains" nil ())
  (api-case-method
    (:get (let ((d (potato.core:load-domains-for-user (potato.core:current-user))))
            (mapcar (lambda (v) (st-json:jso "id" (first v) "name" (second v) "type" (symbol-name (third v)))) d)))))

(define-api-method (api-domain-screen "/domains/([^/]+)" t (domain-id))
  (api-case-method
    (:get (lofn:with-checked-parameters ((include-groups :type :boolean)
                                         (include-channels :type :boolean))
            (api-load-domain-info domain-id include-groups include-channels)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User API calls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-api-method (api-channel-users-screen "/channel/([a-z0-9]+)/users" t (channel-id))
  (api-case-method
    (:get (let* ((channel (potato.core:load-channel-with-check channel-id :if-not-joined :load))
                 (result (potato.core:user-descriptions-for-channel-members channel)))
            (st-json:jso "members" (mapcar #'(lambda (v)
                                               (destructuring-bind (id description nickname user-image)
                                                   v
                                                 (st-json:jso "id" id
                                                              "description" description
                                                              "nickname" nickname
                                                              "image_name" user-image)))
                                           result))))))

(define-api-method (api-download-user-image "/users/([^/]+)/image" t (uid) :result-as-json nil)
  (potato.user-image:download-user-image uid))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Message API calls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-message-modification-allowed (user channel message)
  (unless (or (equal (potato.core:user/id user) (potato.core:message/from message))
              (potato.core:user-is-admin-in-group-p (potato.core:channel/group channel) user))
    (raise-api-error "Permission denied" hunchentoot:+http-forbidden+)))

(define-api-method (api-update-message-screen "/message/([a-zA-Z0-9:_.-]+)" t (message-id))
  (let* ((user (potato.core:current-user))
         (message (potato.core:load-message message-id))
         (channel (potato.core:load-channel-with-check (potato.core:message/channel message) :if-not-joined :load)))
    (api-case-method
      (:put (progn
              (check-message-modification-allowed user channel message)
              (json-bind ((text "text")) (parse-and-check-input-as-json)
                (check-message-length (length text))
                (potato.core:save-message-modification message user text nil nil nil)
                (st-json:jso "result" "ok"
                             "id" (potato.core:message/id message)))))
      (:delete (progn
                 (log:trace "Attempting to delete message: ~s" message-id)
                 (check-message-modification-allowed user channel message)
                 (potato.core:save-message-modification message user "Deleted" nil nil t)
                 (st-json:jso "result" "ok"
                              "id" (potato.core:message/id message))))
      (:get (let* ((accept (hunchentoot:header-in* "accept"))
                   (mode (if accept
                             (string-case:string-case (accept)
                                                      ("text/plain" :text)
                                                      ("text/html" :html)
                                                      ("application/json" :alist)
                                                      (t :text))
                             :text)))
              (potato.core:message-detailed->json message mode (potato.core:current-user)))))))

(define-api-method (api-message-history-screen "/channel/([a-z0-9]+)/history" t (channel-id))
  (let ((channel (potato.core:load-channel-with-check channel-id :if-not-joined :load)))
    (api-case-method
      (:get (lofn:with-parameters (from num format)
              (let ((translate-function (make-translation-function format)))
                (let ((messages (potato.core:load-message-log channel
                                                              (if num (min (parse-integer num) 1000) 10)
                                                              (if (or (null from) (equal from "now")) nil from))))
                  (st-json:jso "messages" (mapcar (lambda (v) (funcall translate-function v)) messages)))))))))

(define-api-method (api-message-screen "/channel/([a-z0-9]+)/create" t (channel-id))
  (api-case-method
    (:post
     (let* ((data (parse-and-check-input-as-json))
            (channel (potato.core:load-channel-with-check channel-id))
            (text (st-json:getjso "text" data)))
       (check-message-length (length text))
       (let ((result (potato.workflow:send-message-to-channel (potato.core:current-user) channel text)))
         (st-json:jso "result" "ok"
                      "id" (getfield :|id| result)))))))

(define-api-method (api-send-file-screen "/channel/([a-z0-9]+)/upload" t (cid))
  (api-case-method
    (:post
     (let ((data (st-json:read-json-from-string (hunchentoot:post-parameter "content"))))
       (log:info "Got data: ~s" data))
     (destructuring-bind (uploaded-file uploaded-name uploaded-content-type)
         (hunchentoot:post-parameter "body")
       (let ((message (potato.upload:process-multipart-upload potato.upload::*file-upload-manager*
                                                              cid
                                                              uploaded-name
                                                              uploaded-content-type
                                                              uploaded-file)))
         (st-json:jso "result" "ok"
                      "id" (potato.core:message/id message)))))))

(define-api-method (api-type-screen "/channel/([a-z0-9]+)/type" t (cid))
  (api-case-method
    (:post (let ((channel (potato.core:load-channel-with-check cid)))
             (json-bind ((active-p "state")) (parse-and-check-input-as-json)
               (if active-p
                   (potato.core:send-typing-start-notification-to-state-server channel (potato.core:current-user))
                   (potato.core:send-typing-end-notification-to-state-server channel (potato.core:current-user)))
               (st-json:jso "result" "ok"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Channel management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun groups-from-domain (domain)
  (loop
    for group in (potato.core:find-groups-in-domain domain)
    when (potato.core:user-role-for-group group (potato.core:current-user))
      collect (st-json:jso "id" (potato.core:group/id group)
                           "name" (potato.core:group/name group)
                           "channels" (api-load-channels-for-group group))))

(defun group-and-channels-from-domain (domain)
  (st-json:jso "id" (potato.core:domain/id domain)
               "name" (potato.core:domain/name domain)
               "groups" (groups-from-domain domain)))

(define-api-method (api-channels-screen "/channels" nil ())
  (api-case-method
    (:get
     (loop
       for domain-user in (potato.core:load-domains-for-user (potato.core:current-user))
       for domain = (potato.db:load-instance 'potato.core:domain (potato.core:domain-user/domain domain-user))
       collect (group-and-channels-from-domain domain)))))

(define-api-method (api-channels-for-domain-screen "/domain/([^/]+)/channels" t (domain-id))
  (api-case-method
    (:get
     (let ((domain (potato.db:load-instance 'potato.core:domain domain-id :error-if-not-found nil)))
       (unless (and domain
                    (potato.core:user-is-in-domain-p domain (potato.core:current-user)))
         (raise-api-error "Domain does not exist" hunchentoot:+http-not-found+))
       (st-json:jso "groups" (groups-from-domain domain))))))

(define-api-method (api-channels2-screen "/channels2" nil ())
  (api-case-method
    (:get
     (let* ((uid (potato.core:user/id (potato.core:current-user)))
            (domains (potato.core:load-domains-for-user uid))
            (result (clouchdb:invoke-view "user" "channels_by_domain_and_user"
                                          :start-key (list uid nil)
                                          :end-key (list uid 'clouchdb:json-map)))
            (rows (getfield :|rows| result))
            (counterpart-ids (loop
                               for v in rows
                               for counterpart = (nth 6 (getfield :|value| v))
                               when counterpart
                                 collect counterpart))
            (counterpart-names (mapcar #'cons counterpart-ids
                                       (potato.core:find-descriptions-for-users counterpart-ids))))
       (let ((current-domain nil)
             (current-channels nil)
             (tree nil)
             (found-domains nil))
         (labels ((collect-domain (domain channels)
                    (push (st-json:jso "id" (potato.core:domain/id domain)
                                       "name" (potato.core:domain/name domain)
                                       "domain-type" (symbol-name (potato.core:domain/domain-type domain))
                                       "channels" channels)
                          tree))
                  (collect-current-domain ()
                    (when current-channels
                      (collect-domain current-domain (reverse current-channels))
                      (push (potato.core:domain/id current-domain) found-domains)
                      (setq current-channels nil))))
           ;; Go thorugh the list of channels and build the domain tree
           (loop
             for row in rows
             do (destructuring-bind (cid name hide group group-type unread-count cpt-id)
                    (getfield :|value| row)
                  (let ((domain-id (second (getfield :|key| row))))
                    (when (or (null current-domain)
                              (not (equal (potato.core:domain/id current-domain) domain-id)))
                      (when current-domain
                        (collect-current-domain))
                      (setq current-domain (potato.db:load-instance 'potato.core:domain domain-id)))
                    (push (st-json:jso "id" cid
                                       "name" (if cpt-id
                                                  (or (cdr (assoc cpt-id counterpart-names
                                                                  :test #'string=))
                                                      (error "No cpt name"))
                                                  name)
                                       "hide" (st-json:as-json-bool hide)
                                       "group" group
                                       "group_type" group-type
                                       "unread_count" unread-count
                                       "private_user" (or cpt-id :null))
                          current-channels)))
             finally (collect-current-domain))
           ;; At this point, there may be some domains left that has
           ;; no channels, add them to the end.
           (loop
             for domain-user in domains
             for domain-id = (potato.core:domain-user/domain domain-user)
             unless (member domain-id found-domains :test #'equal)
               do (collect-domain (potato.db:load-instance 'potato.core:domain domain-id) nil))
           ;; Create the result tree
           (st-json:jso "domains" (reverse tree))))))))

(define-api-method (api-channel-info-screen "/channel/([a-z0-9]+)" t (cid))
  (api-case-method
    (:get (let ((channel (potato.core:load-channel-with-check cid)))
            (st-json:jso "id" (potato.core:channel/id channel)
                         "name" (potato.core:channel/name channel)
                         "topic" (potato.core:channel/topic channel)
                         "group" (potato.core:channel/group channel)
                         "domain" (potato.core:channel/domain channel))))))

(define-api-method (api-create-channel-screen "/channel/create" nil ())
  "Create a new channel. Input is a JSON structure of the following form:
    {name: nnn, topic: nnn, group: nnn}
name and group are required, while the topic parameter is optional."
  (api-case-method
    (:post (json-bind ((gid "group") (name "name") (topic "topic" :required nil))
               (parse-and-check-input-as-json)
             (let ((channel (potato.workflow:create-channel-with-check (potato.core:current-user) gid name topic)))
               (st-json:jso "id" (potato.core:channel/id channel)))))))

(defun make-translation-function (format-name)
  (if format-name
      (string-case:string-case (format-name)
        ("html" #'potato.core:notification-message-cd->json-html)
        ("json" #'potato.core:notification-message-cd->json-alist)
        ("text" #'potato.core:notification-message-cd->json-text)
        (t (raise-api-error "Illegal format: ~a" format-name)))
      #'potato.core:notification-message-cd->json-html))

(defun api-get-single-update (channels event-id format-name services sid)
  #+nil(:content-p t :user-state-p t :user-notifications-p t :unread-p t :channel-updates-p t)
  (potato.rabbitmq-notifications:process-long-poll channels event-id sid services
                                                   (make-translation-function format-name)
                                                   (lambda (queue notifications)
                                                     (st-json:jso "event" queue
                                                                  "data" notifications))))

(defun parse-service-names (service-names)
  (let ((services (loop
                     for part in (split-sequence:split-sequence #\, service-names)
                     append (string-case:string-case (part)
                              ("content" '(:content-p t))
                              ("state" '(:user-state-p t))
                              ("notifications" '(:user-notifications-p t))
                              ("unread" '(:unread-p t))
                              ("channel" '(:channel-updates-p t))
                              ("session" '(:session-p t))
                              (t (raise-api-error (format nil "Illegal service name: '~a'" part)
                                                  hunchentoot:+http-bad-request+))))))
    (unless services
      (raise-api-error "Must specify at least one service type" hunchentoot:+http-bad-request+))
    services))

(define-api-method (api-channel-updates-screen "/channel/([^/]+)/updates" t (cid))
  (lofn:with-checked-parameters ((event-id :name "event-id" :required nil)
                                 (format-name :name "format" :required nil)
                                 (service-names :name "services" :required nil)
                                 (sid :name "session_id" :required nil))
    (let ((channel (potato.core:load-channel-with-check cid  :if-not-joined :join))
          (services (if service-names
                        (parse-service-names service-names)
                        '(:content-p t))))
      (api-case-method
        (:get (api-get-single-update (list channel) event-id format-name services sid))))))

(define-api-method (api-channel-updates2-screen "/channel-updates" nil ())
  (lofn:with-checked-parameters ((event-id :name "event-id" :required nil)
                                 (channel-names :name "channels" :required nil)
                                 (format-name :name "format" :required nil)
                                 (service-names :name "services" :required nil)
                                 (sid :name "session_id" :required nil))
    (let* ((services (if service-names
                         (parse-service-names service-names)
                         '(:content-p t)))
           (cids (split-sequence:split-sequence #\, channel-names))
           (channels (mapcar (lambda (cid)
                               (potato.core:load-channel-with-check cid :if-not-joined :join))
                             cids)))
      (api-case-method
        (:get (api-get-single-update channels event-id format-name services sid))))))

(define-api-method (api-channel-updates2-control-screen "/channel-updates/update" nil ())
  (lofn:with-checked-parameters ((event-id :name "event-id" :required t)
                                 (command :name "cmd" :required t)
                                 (cid :name "channel" :required t)
                                 (service-names :name "services" :required nil))
    (api-case-method
      (:post
       (let ((services (if service-names
                           (parse-service-names service-names)
                           '(:content-p t)))
             (channel (potato.core:load-channel-with-check cid :if-not-joined :join)))
         (unless (equal command "add")
           (raise-api-error "Only command add is currently supported" hunchentoot:+http-bad-request+))
         (destructuring-bind (&key content-p user-state-p channel-updates-p &allow-other-keys) services
           (unless (or content-p user-state-p channel-updates-p)
             (raise-api-error "Illegal services: at least one of content, state, channel is required"
                              hunchentoot:+http-bad-request+))
           (potato.rabbitmq-notifications:verify-queue-name event-id nil)
           (with-pooled-rabbitmq-connection (conn)
             (potato.rabbitmq-notifications:add-new-channel-binding conn 1 event-id (potato.core:channel/id channel)
                                                                    :content-p content-p
                                                                    :user-state-p user-state-p
                                                                    :channel-updates-p channel-updates-p))
           (st-json:jso "result" "ok")))))))

(define-api-method (api-download-screen "/download/([^/]+)" t (key) :result-as-json nil)
  (api-case-method
    (:get
     (let ((file (potato.db:load-instance 'potato.upload:file key :error-if-not-found nil)))
       (unless (and file
                    (potato.upload:file/confirmed-p file)
                    (potato.core:user-is-in-channel-p (potato.upload:file/channel file) (potato.core:current-user)))
         (raise-api-error "File does not exist" hunchentoot:+http-not-found+))
       (potato.upload:download-file-to-client file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Notifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-api-method (api-mark-notifications "/channel/([^/]+)/clear-notifications" t (cid))
  (api-case-method
    (:post
     (let ((channel (potato.core:load-channel-with-check cid :if-not-joined :load)))
       (potato.user-notification:mark-notifications-for-user-channel (potato.core:current-user) channel)
       (st-json:jso "result" "ok")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-api-method (api-search-channel-screen "/channel/([^/]+)/search" t (cid))
  (api-case-method
    (:get
     (lofn:with-checked-parameters ((query :name "query" :type :string :required true :allow-blank nil :trimmed true)
                                    (star-only-p :name "star-only" :type :boolean :required nil))
       (let ((channel (potato.core:load-channel-with-check cid)))
         (potato.search:search-messages-json query (potato.core:channel/id channel) nil star-only-p))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Slashcommand
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-api-method (api-send-slashcommand "/command" nil ())
  (api-case-method
    (:post
     (potato.slashcommand:process-incoming-slashcommand (parse-and-check-input-as-json)))))
