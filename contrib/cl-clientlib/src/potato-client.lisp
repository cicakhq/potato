(in-package :potato-client)

(define-condition potato-client-error (error)
  ())

(define-condition request-error (potato-client-error)
  ((code :type integer
         :initarg :code
         :reader request-error/code)
   (reason :type string
           :initarg :reason
           :reader request-error/reason))
  (:report (lambda (condition out)
             (format out "Error from server. code: ~a, reason: ~a"
                     (request-error/code condition)
                     (request-error/reason condition)))))

(defclass connection ()
  ((api-key    :type string
               :initform (error "~s not specified when creating new ~s" :api-key 'connection-state)
               :initarg :api-key
               :reader connection/api-key)
   (url-prefix :type string
               :initform "https://potato.dhsdevelopments.com/"
               :initarg :url-prefix
               :reader connection/url-prefix)
   (event-id   :type (or null string)
               :initform nil
               :accessor connection/event-id)))

(defvar *connection* nil
  "The current default connection")

(defun make-potato-url (conn suffix)
  (let ((prefix (connection/url-prefix conn)))
    (format nil "~a~aapi/1.0~a"
            prefix
            (if (eql (aref prefix (1- (length prefix))) #\/) "" "/")
            suffix)))

(defun authenticated-request (conn suffix &key (method :get) content params)
  (multiple-value-bind (content code headers uri stream should-close reason)
      (drakma:http-request (make-potato-url conn suffix)
                           :additional-headers `(("API-Token" . ,(connection/api-key conn)))
                           :parameters params
                           :want-stream t
                           :method method
                           :content content
                           :force-binary t)
    (declare (ignore content headers uri))
    (unwind-protect
         (progn
           (unless (= code 200)
             (error 'request-error :code code :reason reason))
           (st-json:read-json (flexi-streams:make-flexi-stream stream :external-format :utf-8)))
      (when should-close (close stream)))))

(defun list-domains (&key (connection *connection*))
  (check-type connection connection)
  (let ((res (authenticated-request connection "/domains")))
    (loop
      for domain in res
      collect (list (st-json:getjso "id" domain)
                    (st-json:getjso "name" domain)
                    (st-json:getjso "type" domain)))))

(defun load-channel (channel-id &key (connection *connection*))
  (check-type channel-id string)
  (check-type connection connection)
  (let* ((res (authenticated-request connection (format nil "/channel/~a" channel-id)))
         (private-user (st-json:getjso "private_user" res)))
    `((:id . ,(st-json:getjso "id" res))
      (:name . ,(st-json:getjso "name" res))
      (:topic . ,(st-json:getjso "topic" res))
      (:group . ,(st-json:getjso "group" res))
      (:group-type . ,(st-json:getjso "group_type" res))
      (:domain . ,(st-json:getjso "domain" res))
      (:private-user . ,(if (eq private-user :null) nil private-user)))))

(defun load-channel-tree (&key (connection *connection*))
  (check-type connection connection)
  (let ((res (authenticated-request connection "/channels2")))
    (loop
      for domain in (st-json:getjso "domains" res)
      collect `((:id . ,(st-json:getjso "id" domain))
                (:name . ,(st-json:getjso "name" domain))
                (:type . ,(intern (st-json:getjso "domain-type" domain) "KEYWORD"))
                (:channels . ,(loop
                                for channel in (st-json:getjso "channels" domain)
                                collect `((:id . ,(st-json:getjso "id" channel))
                                          (:name . ,(st-json:getjso "name" channel))
                                          (:hide . ,(st-json:getjso "hide" channel))
                                          (:group . ,(st-json:getjso "group" channel))
                                          (:group-type . ,(intern (st-json:getjso "group_type" channel) "KEYWORD"))
                                          (:unread-count . ,(st-json:getjso "unread_count" channel))
                                          ,@(let ((private-user (st-json:getjso "private_user" channel)))
                                              (if (not (eq private-user :null))
                                                  `((:private-user . ,private-user)))))))))))

(defun send-message (channel text &key (connection *connection*))
  (check-type channel string)
  (check-type text string)
  (check-type connection connection)
  (let* ((content (st-json:jso "text" text))
         (res (authenticated-request connection (format nil "/channel/~a/create" channel)
                                     :method :post
                                     :content (babel:string-to-octets (st-json:write-json-to-string content)
                                                                      :encoding :utf-8))))
    (unless (equal (st-json:getjso "result" res) "ok")
      (error "Error while posting message"))
    (values (st-json:getjso "id" res))))

(defun list-users (channel-id &key (connection *connection*))
  (check-type channel-id string)
  (check-type connection connection)
  (let ((res (authenticated-request connection (format nil "/channel/~a/users" channel-id))))
    (loop
      for user in (st-json:getjso "members" res)
      collect `((:id . ,(st-json:getjso "id" user))
                (:description . ,(st-json:getjso "description" user))
                (:nickname . ,(st-json:getjso "nickname" user))
                (:image-name . ,(st-json:getjso "image_name" user))))))

(defun start-channel-listener (channel-id callback &key (connection *connection*))
  (check-type channel-id string)
  (check-type callback function)
  (check-type connection connection)
  )

(defun listener-loop (connection cid-list callback-fn)
  (loop
    with event-id = nil
    for res = (authenticated-request connection "/channel-updates"
                                     :params `(,@(if event-id `(("event-id" . ,event-id)))
                                               ("channels" . ,(format nil "~{~a~^,~}" cid-list))
                                               ("format" . "json")
                                               ("services" . "content,state,notifications")
                                               ("session_id" . "foo")))
    do (progn
         (setq event-id (st-json:getjso "event" res))
         (setf (connection/event-id connection) event-id)
         (loop
           for event in (st-json:getjso "data" res)
           do (funcall callback-fn event)))))

(defun subscribe-to-channel (channel-id &key (connection *connection*))
  (check-type channel-id string)
  (check-type connection connection)
  (let ((event-id (connection/event-id connection)))
    (unless event-id
      (error "event-id is not set in connection"))
    (let ((res (authenticated-request connection "/channel-updates/update"
                                      :method :post
                                      :params `(("event-id" . ,event-id)
                                                ("cmd" . "add")
                                                ("channel" . ,channel-id)
                                                ("services" . "content,state")))))
      (unless (equal (st-json:getjso "result" res) "ok")
        (error "Error while adding subscription to channel")))))

(defun load-user (uid &key (connection *connection*))
  (check-type uid string)
  (check-type connection connection)
  (let ((res (authenticated-request connection (format nil "/users/~a" uid))))
    res))
