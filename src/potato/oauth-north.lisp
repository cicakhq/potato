(in-package :potato.oauth-north)

(declaim #.potato.common::*compile-decl*)

(defun make-nonce ()
  (potato.common:make-random-name 40))

(defclass potato-north-server (north:server)
  ())

(defclass potato-north-application (north:application)
  ())

(defclass oauth-session (north:session)
  ())

(defun make-potato-north-oauth-session-id (token)
  (format nil "potatonorthoauthsession-~a" token))

(defmethod north:make-session ((server potato-north-server) application callback &key (access nil access-set-p))
  (let ((session (apply #'make-instance 'oauth-session
                        :token (make-nonce)
                        :token-secret (make-nonce)
                        :verifier (potato.common:make-random-name 10)
                        :key (north:key application)
                        :callback callback
                        (if access-set-p (list :access access)))))
    (clouchdb:create-document `((:|token|        . ,(north:token session))
                                (:|token_secret| . ,(north:token-secret session))
                                (:|verifier|     . ,(north:verifier session) )
                                (:|callback|     . ,(north:callback session))
                                (:|key|          . ,(north:key session))
                                (:|access|       . ,(symbol-name (north:access session)))
                                (:|type|         . "potato_oauth_session"))
                              :id (make-potato-north-oauth-session-id (north:token session)))
    session))

(defmethod north:session ((server potato-north-server) token)
  (let ((doc (clouchdb:get-document (make-potato-north-oauth-session-id token))))
    (unless (equal (getfield :|type| doc) "potato_oauth_session")
      (error "Got wrong document type when looking up session"))
    (make-instance 'oauth-session
                   :token (getfield :|token| doc)
                   :token-secret (getfield :|token_secret| doc)
                   :verifier (getfield :|verifier| doc)
                   :callback (getfield :|callback| doc)
                   :key (getfield :|key| doc)
                   :access (intern (getfield :|access| doc) "KEYWORD"))))

(defun make-potato-north-application-id (key)
  (format nil "potatonorthapplication-~a" key))

(defmethod north:make-application ((server potato-north-server) &key name)
  (let ((app (make-instance 'potato-north-application
                            :key (make-nonce)
                            :secret (make-nonce)
                            :name name)))
    (clouchdb:create-document `((:|key|    . ,(north:key app))
                                (:|secret| . ,(north:secret app))
                                (:|name|   . ,(north:name app))
                                (:|type|   . "potato_north_application"))
                              :id (make-potato-north-application-id (north:key app)))
    app))

(defmethod north:application ((server potato-north-server) application-key)
  (let ((doc (clouchdb:get-document (make-potato-north-application-id application-key))))
    (unless (equal (getfield :|type| doc) "potato_north_application")
      (error "Got wrong document type when looking up oauth application"))
    (make-instance 'potato-north-application
                   :key (getfield :|key| doc)
                   :secret (getfield :|secret| doc)
                   :name (getfield :|name| doc))))

(defmethod north:rehash-session ((server potato-north-server) session)
  )

(defmethod north:revoke-application ((server potato-north-server) application-key)
  (clouchdb:delete-document (make-potato-north-application-id application-key)))

(defmethod north:revoke-session ((server potato-north-server) token)
  (clouchdb:delete-document (make-potato-north-oauth-session-id token)))

;;; TODO: The below two methods needs to be implemented in order to
;;; prevent mitm attacks. This will be implemented using a separate
;;; service.

(defmethod north:record-nonce ((server potato-north-server) timestamp nonce)
  nil)

(defmethod north:find-nonce ((server potato-north-server) timestamp nonce)
  nil)

(defvar *server* (make-instance 'potato-north-server))

(defun make-request (request)
  (make-instance 'north:request
                 :http-method (hunchentoot:request-method* request)
                 :url (north::normalize-url
                       (format nil "~a~a" potato:*external-listen-address* (hunchentoot:request-uri* request)))
                 :parameters (append (hunchentoot:post-parameters* request)
                                     (hunchentoot:get-parameters* request)) 
                 :headers (hunchentoot:headers-in* request)))

(lofn:define-handler-fn (oauth-request-token "/oauth/request-token" nil ())
  (multiple-value-bind (token secret callback-confirmed)
      (north:oauth/request-token *server* (make-request hunchentoot:*request*))
    (setf (hunchentoot:content-type*) "text/plain")
    (north:alist->oauth-response `(("oauth_token" . ,token)
                                   ("oauth_token_secret" . ,secret)
                                   ("oauth_callback_confirmed" . ,(if callback-confirmed :true :false))))))

(lofn:define-handler-fn (oauth-authorise "/oauth/authorize" nil ())
  (lofn:with-checked-parameters ((oauth-token :name "oauth_token" :required t :allow-blank nil)
                                 (verifier :name "verifier")
                                 (error-link :name "error"))
    (let* ((session (or (north:session *server* oauth-token)
                        (error 'north:invalid-token :request (make-request hunchentoot:*request*))))
           (application (north:application *server* (north:key session))))
      (setf (hunchentoot:content-type*) "application/html")
      (lofn:show-template-stream "oauth-authorise.tmpl"
                                 `((:oauth-token . ,oauth-token)
                                   (:application . ,(north:name application))
                                   (:verifier . ,verifier)
                                   (:error . ,error-link))))))

(lofn:define-handler-fn (oauth-authenticate "/oauth/authenticate" nil ())
  (lofn:with-checked-parameters (action)
    (string-case:string-case (action)
      ("deny" "Not allowed")
      ("allow" (multiple-value-bind (token verifier url)
                   (north:oauth/authorize *server* (make-request hunchentoot:*request*))
                 (if url
                     (hunchentoot:redirect url)
                     (hunchentoot:redirect (format nil "/oauth/authorize?oauth_token=~a&verifier=~a"
                                                   (north:url-encode token) (north:url-encode verifier))))))
      (t
       (hunchentoot:redirect "/oauth/authorize?error=Invalid_action")))))

(lofn:define-handler-fn (oauth-access-token "/oauth/access-token" nil ())
  (multiple-value-bind (token secret)
      (north:oauth/access-token *server* (make-request hunchentoot:*request*))
    (north:alist->oauth-response `(("oauth_token" . ,token)
                                   ("oauth_token_secret" . ,secret)))))

(lofn:define-handler-fn (oauth-verify "/oauth/verify" nil ())
  (north:oauth/verify *server* (make-request hunchentoot:*request*))
  (north:alist->oauth-response `(("status" . "success"))))
