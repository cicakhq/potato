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

(defun load-user-from-api-token-or-session ()
  (let ((api-token (hunchentoot:header-in* "api-token")))
    (if api-token
        (load-user-from-api-token api-token)
        (let ((user (potato.core:validate-cookie-and-find-user)))
          (unless user
            (raise-api-error "Not logged in" hunchentoot:+http-forbidden+))
          user))))

(defun verify-api-token-and-run (url fn)
  (handler-case
      (let ((potato.core::*current-auth-user* (load-user-from-api-token-or-session)))
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
               (list "groups" (groups-from-domain domain include-channels-p))))))

(defun parse-and-check-input-as-json ()
  (let ((json-text (hunchentoot:raw-post-data :force-text t)))
    (when (null json-text)
      (raise-api-error "Empty input" hunchentoot:+http-not-acceptable+))
    (let ((data (st-json:read-json-from-string json-text)))
      data)))

(defun check-message-length (length)
  (when (> length potato.core:*max-message-size*)
    (raise-api-error "Message is too large" hunchentoot:+http-request-entity-too-large+)))

(defun group-as-json (group include-channels-p)
  (apply #'st-json:jso
         "id" (potato.core:group/id group)
         "name" (potato.core:group/name group)
         "type" (symbol-name (potato.core:group/type group))
         (if include-channels-p
             (list "channels" (api-load-channels-for-group group)))))

(define-api-method (api-version-screen "/version" nil ())
  (api-case-method
    (:get (st-json:jso "version" "1"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Session API calls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-api-method (api-user-init-screen "/user-session" nil ())
  "This API call returns all the typical information a client needs.
The purpose of this call is to reduce the number of roundtrips needed
to initialise a session."
  (api-case-method
    (:get
     (let ((user (potato.core:current-user)))
       (apply #'st-json:jso
              "user" (user-as-json user)
              "domains" (load-domain-and-channel-information-as-json user)
              "websocket_url" *external-websocket-listen-address*
              "upload_location" (ecase potato.upload:*default-upload-location*
                                  (:s3 "s3)")
                                  (:file "file")
                                  ((nil) :null))
              (if (and *s3-browser-access-key* *s3-endpoint* *s3-bucket*)
                  (st-json:jso "s3_credentials" (st-json:jso "access_key" *s3-browser-access-key*
                                                             "endpoint" *s3-endpoint* *s3-bucket*
                                                             "bucket" *s3-bucket*))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Domain API calls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-api-method (api-joined-domains-screen "/domains" nil ())
  (api-case-method
    (:get (loop
            for domain-user in (potato.core:load-domains-for-user (potato.core:current-user))
            for domain = (potato.db:load-instance 'potato.core:domain (potato.core:domain-user/domain domain-user))
            collect (st-json:jso "id" (potato.core:domain/id domain)
                                 "name" (potato.core:domain/name domain)
                                 "type" (symbol-name (potato.core:domain-user/role domain-user)))))))

(define-api-method (api-domain-screen "/domains/([^/]+)" t (domain-id))
  (api-case-method
    (:get (lofn:with-checked-parameters ((include-groups :type :boolean)
                                         (include-channels :type :boolean))
            (api-load-domain-info domain-id include-groups include-channels)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Group API calls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-api-method (api-group-screen "/groups/([^/]+)" t (group-id))
  (api-case-method
    (:get (lofn:with-checked-parameters ((include-channels :type :boolean))
            (let ((group (potato.core:load-group-with-check group-id)))
              (group-as-json group include-channels))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User API calls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun user-as-json (user)
  (st-json:jso "id" (potato.core:user/id user)
               "description" (potato.core:user/description user)
               "nickname" (potato.core:user/nickname user)
               "image_name" (potato.core:user/image-name user)))

(define-api-method (api-channel-users-screen "/channel/([a-z0-9]+)/users" t (channel-id))
  (api-case-method
    (:get (let* ((channel (potato.core:load-channel-with-check channel-id :if-not-joined :load))
                 (result (potato.core:user-descriptions-for-channel-members channel)))
            (st-json:jso "members" (mapcar (lambda (v)
                                             (destructuring-bind (id description nickname user-image)
                                                 v
                                               (st-json:jso "id" id
                                                            "description" description
                                                            "nickname" nickname
                                                            "image_name" user-image)))
                                           result))))))

(define-api-method (api-user-info "/users/([^/]+)" t (uid))
  (api-case-method
    (:get
     (let ((user (potato.db:load-instance 'potato.core:user uid :error-if-not-found nil)))
       (if (and user
                (potato.core:common-user-domains (potato.core:current-user) uid))
           (st-json:jso "user" (user-as-json user))
           (raise-api-error "User not found" hunchentoot:+http-not-found+))))))

(define-api-method (api-download-user-image "/users/([^/]+)/image" t (uid) :result-as-json nil)
  (api-case-method
    (:get
     (labels ((raise-not-found ()
                (raise-api-error "User not found" hunchentoot:+http-not-found+)))
       (unless (potato.core:common-user-domains (potato.core:current-user) uid)
         (raise-not-found))
       (alexandria:if-let ((content (potato.user-image:user-load-image uid)))
         (progn
           (setf (hunchentoot:content-type*) "image/png")
           (setf (hunchentoot:header-out "cache-control") "public,max-age=86400")
           content)
         ;; ELSE: User not found or no image exists
         (raise-not-found))))))

(define-api-method (api-groups-for-user-screen "/users/([^/]+)/groups" t (uid))
  (api-case-method
    (:get
     (let* ((own-uid (potato.core:user/id (potato.core:current-user)))
            (loading-own-user-p (equal own-uid uid))
            (groups (clouchdb:invoke-view "group" "groups_for_user_nodomain"
                                          :start-key (list uid nil)
                                          :end-key (list uid 'clouchdb:json-map)))
            (visible-group-ids (if loading-own-user-p
                                   nil
                                   (let ((visible-groups (clouchdb:invoke-view "group" "groups_for_user_nodomain"
                                                                               :start-key (list own-uid nil)
                                                                               :end-key (list own-uid 'clouchdb:json-map))))
                                     ;; TODO: Merge this with the list of groups that are in domains
                                     ;;       for which the user is admin.
                                     (mapcar (lambda (row)
                                               (getfield :|group| (getfield :|value| row)))
                                             (getfield :|rows| visible-groups))))))
       (loop
         for row in (getfield :|rows| groups)
         for value = (getfield :|value| row)
         for gid = (getfield :|group| value)
         when (or loading-own-user-p (member gid visible-group-ids :test #'equal))
           collect (st-json:jso "id" gid
                                "name" (getfield :|group_name| value)
                                "type" (getfield :|group_type| value)
                                "role" (getfield :|role| value)))))))

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

(define-api-method (api-leave-channel-screen "/channel/([a-z0-9]+)/leave" t (cid))
  (api-case-method
    (:post
     (let ((channel (potato.core:load-channel-with-check cid :if-not-joined :ignore)))
       (when channel
         (potato.core:remove-user-from-channel channel (potato.core:current-user)))
       (st-json:jso "result" "ok"
                    "detail" (if channel "leave_success" "was_not_joined"))))))

(define-api-method (api-set-channel-visibility-screen "/channel/([a-z0-9]+)/show" t (cid))
  (api-case-method
    (:post
     (json-bind ((show "show" :required t :type :boolean))
         (parse-and-check-input-as-json)
       (let ((channel (potato.core:load-channel-with-check cid)))
         (potato.core:update-channel-visibility-for-user channel (potato.core:current-user) show)
         (st-json:jso "result" "ok"))))))

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

(define-api-method (api-private-channel-for-user-screen "/private/([^/]+)/([^/]+)" t (domain-id uid))
  (api-case-method
    (:post
     (let* ((domain (potato.core:load-domain-with-check domain-id (potato.core:current-user)))
            (channel (potato.private:find-private-channel-for-users domain uid (potato.core:current-user))))
       (st-json:jso "channel" (potato.core:channel/id channel))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Channel management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun groups-from-domain (domain include-channels-p)
  (loop
    for group in (potato.core:find-groups-in-domain domain)
    when (potato.core:user-role-for-group group (potato.core:current-user))
      collect (group-as-json group include-channels-p)))

(defun group-and-channels-from-domain (domain)
  (st-json:jso "id" (potato.core:domain/id domain)
               "name" (potato.core:domain/name domain)
               "groups" (groups-from-domain domain t)))

(define-api-method (api-channels-screen "/channels" nil ())
  (api-case-method
    (:get
     (loop
       for domain-user in (potato.core:load-domains-for-user (potato.core:current-user))
       for domain = (potato.db:load-instance 'potato.core:domain (potato.core:domain-user/domain domain-user))
       collect (group-and-channels-from-domain domain)))))

(defun load-domain-and-channel-information-as-json (user)
  (let* ((uid (potato.core:user/id user))
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
        (reverse tree)))))

(define-api-method (api-channels2-screen "/channels2" nil ())
  (api-case-method
    (:get
     (st-json:jso "domains" (load-domain-and-channel-information-as-json (potato.core:current-user))))))

(define-api-method (api-channel-info-screen "/channel/([a-z0-9]+)" t (cid))
  (api-case-method
    (:get
     (multiple-value-bind (channel-users channel)
         (potato.core:load-channel-users-with-check cid :if-not-joined :load)
       (let ((group (potato.db:load-instance 'potato.core:group (potato.core:channel/group channel)))
             (u (getfield (intern (potato.core:user/id (potato.core:current-user)) "KEYWORD")
                          (potato.core:channel-users/users channel-users)
                          :accept-missing t)))
         (st-json:jso "id" (potato.core:channel/id channel)
                      "name" (potato.core:channel/name channel)
                      "topic" (potato.core:channel/topic channel)
                      "group" (potato.core:channel/group channel)
                      "group_type" (symbol-name (potato.core:group/type group))
                      "unread_count" (or (and u (getfield :|count| u)) 0)
                      "domain" (potato.core:channel/domain channel)
                      "private_user" (if (eq (potato.core:group/type group) :private)
                                         (potato.private:find-chat-counterpart channel-users (potato.core:current-user))
                                         :null)))))
    ;; Create a new channel. Only valid if the channel ID is set to "create", which is not a valid channel ID
    (:put
     (unless (equal cid "create")
       (raise-api-error "Incorrect name when creating channel" :code hunchentoot:+http-not-found+))
     (json-bind ((domain-id "domain" :required nil)
                 (gid "group" :required nil)
                 (name "name")
                 (topic "topic" :required nil))
         (parse-and-check-input-as-json)
       (when (or (and (null domain-id) (null gid))
                 (and domain-id gid))
         (raise-api-error "Either group or domain needs to be specified" :code hunchentoot:+http-bad-request+))
       (let* ((group (if gid
                         (potato.db:load-instance 'potato.core:group gid)
                         (potato.core:find-default-group-in-domain domain-id)))
              (channel (potato.workflow:create-channel-with-check (potato.core:current-user) group name topic)))
         (st-json:jso "id" (potato.core:channel/id channel)
                      "name" (potato.core:channel/name channel)
                      "group" (potato.core:channel/group channel)
                      "group_type" (symbol-name (potato.core:group/type group))
                      "domain" (potato.core:channel/domain channel)
                      "private_user" :null))))))

#+nil
(define-api-method (api-create-channel-screen "/channel/new/create" nil ())
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
                                                                  "data" notifications))
                                                   t))

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
           (cids (if (or (null channel-names) (equal channel-names ""))
                     nil
                     (split-sequence:split-sequence #\, channel-names)))
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
       (string-case:string-case (command)
         ("add"
          (let ((services (if service-names
                              (parse-service-names service-names)
                              '(:content-p t)))
                (channel (potato.core:load-channel-with-check cid :if-not-joined :join)))
            (destructuring-bind (&key content-p user-state-p channel-updates-p &allow-other-keys) services
              (unless (or content-p user-state-p channel-updates-p)
                (raise-api-error "Illegal services: at least one of content, state, channel is required"
                                 hunchentoot:+http-bad-request+))
              (potato.rabbitmq-notifications:verify-queue-name event-id nil)
              (with-pooled-rabbitmq-connection (conn)
                (potato.rabbitmq-notifications:add-new-channel-binding conn 1 event-id (potato.core:channel/id channel)
                                                                       :content-p content-p
                                                                       :user-state-p user-state-p
                                                                       :channel-updates-p channel-updates-p)))))
         ("remove"
          (potato.rabbitmq-notifications:verify-queue-name event-id nil)
          (format *out* "Removing binding ~s for channel ~s" event-id cid)
          #+nil
          (with-pooled-rabbitmq-connection (conn)
            (let ((channel (potato.core:load-channel-with-check cid)))
              (potato.rabbitmq-notifications:remove-channel-binding conn 1 event-id (potato.core:channel/id channel)))))
         (t
          (raise-api-error "Unknown command" hunchentoot:+http-bad-request+)))
       (st-json:jso "result" "ok")))))

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
