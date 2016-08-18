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
               :reader connection/url-prefix)))

(defvar *connection* nil
  "The current default connection")

(defun make-potato-url (conn suffix)
  (let ((prefix (connection/url-prefix conn)))
    (format nil "~a~aapi/1.0~a"
            prefix
            (if (eql (aref prefix (1- (length prefix))) #\/) "" "/")
            suffix)))

(defun authenticated-request (conn suffix &key (method :get) content)
  (multiple-value-bind (content code headers uri stream should-close reason)
      (drakma:http-request (make-potato-url conn suffix)
                           :additional-headers `(("API-Token" . ,(connection/api-key conn)))
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
  (let ((res (authenticated-request connection "/channels2")))
    (loop
      for domain in (st-json:getjso "domains" res)
      collect `((:id . ,(st-json:getjso "id" domain))
                (:name . ,(st-json:getjso "name" domain))
                (:type . ,(st-json:getjso "domain-type" domain))
                (:channels . ,(loop
                                for channel in (st-json:getjso "channels" domain)
                                collect `((:id . ,(st-json:getjso "id" channel))
                                          (:name . ,(st-json:getjso "name" channel))
                                          (:hide . ,(st-json:getjso "hide" channel))
                                          (:group . ,(st-json:getjso "group" channel))
                                          (:group-type . ,(st-json:getjso "group_type" channel))
                                          (:unread-count . ,(st-json:getjso "unread_count" channel))
                                          ,@(let ((private-user (st-json:getjso "private_user" channel)))
                                              (if (not (eq private-user :null))
                                                  `((:private-user . ,private-user)))))))))))

(defun send-message (channel text &key (connection *connection*))
  (check-type channel string)
  (check-type text string)
  (let* ((content (st-json:jso "text" text))
         (res (authenticated-request connection (format nil "/channel/~a/create" channel)
                                     :method :post
                                     :content (babel:string-to-octets (st-json:write-json-to-string content)
                                                                      :encoding :utf-8))))
    (unless (equal (st-json:getjso "result" res) "ok")
      (error "Error while posting message"))
    (st-json:getjso "id" res)))
