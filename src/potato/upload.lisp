(in-package :potato.upload)

(declaim #.potato.common::*compile-decl*)

(defclass file ()
  ((name            :type string
                    :initarg :name
                    :reader file/name
                    :persisted-p t)
   (key             :type string
                    :initarg :key
                    :reader file/key
                    :persisted-p t)
   (requested-date  :type local-time:timestamp
                    :initform (local-time:now)
                    :reader file/requested-date
                    :persisted-p t
                    :persisted-type :date)
   (verify-date     :type (or null local-time:timestamp)
                    :initform nil
                    :accessor file/verify-date
                    :persisted-p t
                    :persisted-type :date)
   (confirmed-p     :type (or null local-time:timestamp)
                    :initarg :uploaded-date
                    :initform nil
                    :accessor file/confirmed-p
                    :persisted-p t
                    :persisted-type :date)
   (channel         :type string
                    :initarg :channel
                    :reader file/channel
                    :persisted-p t)
   (user            :type string
                    :initarg :user
                    :reader file/user
                    :persisted-p t)
   (size            :type (or null integer)
                    :initarg :size
                    :initform nil
                    :accessor file/size
                    :persisted-p t
                    :persisted-type :integer)
   (message         :type string
                    :initarg :message
                    :reader file/message
                    :initform nil
                    :persisted-p t)
   (content-type    :type (or null string)
                    :initarg :mime-type
                    :initform nil
                    :accessor file/mime-type
                    :persisted-p t
                    :persisted-allow-missing-value t
                    :persisted-missing-default "binary/octet-stream")
   (location        :type (or null keyword)
                    :initarg :location
                    :reader file/location
                    :persisted-p t
                    :persisted-type :symbol
                    :persisted-allow-missing-value t
                    :persisted-missing-default :s3
                    :documentation "Storage location for this file. :S3 or :FILE"))
  (:metaclass potato.db:persisted-entry-class))

(defgeneric file/id (file)
  (:method ((file file)) (potato.db:persisted-entry/couchdb-id file)))

(defun make-file-id (key)
  (concatenate 'string "file-" (string->sha1 key)))

(defmethod initialize-instance :after ((obj file) &key)
  (when (or (not (slot-boundp obj 'potato.db:couchdb-id))
            (null (slot-value obj 'potato.db:couchdb-id)))
    (setf (potato.db:persisted-entry/couchdb-id obj) (make-file-id (file/key obj)))))

(defmethod print-object ((obj file) stream)
  (print-unreadable-safely (name key) obj stream
    (format stream "~s NAME ~s KEY ~s"
            (if (slot-boundp obj 'potato.db:couchdb-id) (slot-value obj 'potato.db:couchdb-id) :not-bound)
            name key)))

(defun compute-s3-upload-policy-signature (encoded)
  (let ((key *s3-browser-secret-key*))
    (let ((hmac (ironclad:make-hmac (babel:string-to-octets key :encoding :utf-8) 'ironclad:sha1)))
      (ironclad:update-hmac hmac (babel:string-to-octets encoded :encoding :utf-8))
      (base64:usb8-array-to-base64-string (ironclad:hmac-digest hmac)))))

(defparameter *condition-key-to-keyword* '(("x-amz-meta-channel" . :channel)
                                           ("x-amz-meta-user" . :user)
                                           ("x-amz-meta-qqfilename" . :filename)
                                           ("key" . :key)
                                           ("Content-Type" . :content-type)
                                           ("acl" . :acl)
                                           ("bucket" . :bucket)))

(defun extract-conditions (data)
  (loop
     for condition in (st-json:getjso "conditions" data)
     append (loop
               for mapping in *condition-key-to-keyword*
               for condition-value = (st-json:getjso (car mapping) condition)
               when condition-value
               return (list (cons (cdr mapping) condition-value)))))

(defun verify-policy-and-create-document (conditions channel)
  (log:trace "Conditions=~s, channel=~s" conditions channel)
  (when (and (string= (getfield :bucket conditions) *s3-bucket*)
             (string= (getfield :acl conditions) "private")
             (string= (hunchentoot:url-decode (getfield :user conditions)) (potato.core:user/id (potato.core:current-user))))
    (alexandria:when-let ((key (getfield :key conditions))
                          (filename (getfield :filename conditions)))
      (let ((file (potato.db:load-instance 'file (make-file-id key))))
        (log:trace "Comparisons: ~s=~s ~s=~s ~s=~s ~s"
                   (hunchentoot:url-decode filename) (file/name file)
                   (potato.core:channel/id channel) (file/channel file)
                   (potato.core:user/id (potato.core:current-user)) (file/user file)
                   (file/verify-date file))
        (when (and (string= (hunchentoot:url-decode filename) (file/name file))
                   (string= (potato.core:channel/id channel) (file/channel file))
                   (string= (potato.core:user/id (potato.core:current-user)) (file/user file))
                   (null (file/verify-date file)))
          (setf (file/verify-date file) (local-time:now))
          (setf (file/mime-type file) (or (getfield :content-type conditions :accept-missing t)
                                          "binary/octet-stream"))
          (potato.db:save-instance file)
          file)))))

(potato.core:define-handler-fn-login (create-file-key-screen "/s3/create_key" nil ())
  (check-s3-active)
  (potato.core:with-authenticated-user ()
    (lofn:with-checked-parameters ((filename :required t :allow-blank nil)
                                   (channel :required t :allow-blank nil)
                                   (message :required nil :allow-blank t :trimmed t))
      (log:trace "Creating key for file: ~s, channel: ~s, message: ~s" filename channel message)
      (potato.core:check-user-in-channel channel)
      (multiple-value-bind (match strings)
          (cl-ppcre:scan-to-strings ".+(\\.[^.]+)$" filename)
        (let ((ext (if match (aref strings 0) "")))
          (let* ((key (format nil "~a/~a/~a~a" *s3-directory* channel (make-random-name 40) ext))
                 (file (make-instance 'file
                                      :name filename
                                      :channel channel
                                      :key key
                                      :user (potato.core:user/id (potato.core:current-user))
                                      :message (or message  "Uploaded file")
                                      :location :s3)))
            (potato.db:save-instance file)
            key))))))

(potato.core:define-json-handler-fn-login (validate-upload-screen "/s3/signature" data nil ())
  (check-s3-active)
  (potato.core:with-authenticated-user ()
    (log:trace "Incoming JSON data: ~s" data)
    (let* ((conditions (extract-conditions data))
           (channel-id (getfield :channel conditions))
           (channel (potato.core:load-channel-with-check channel-id)))

      (setf (hunchentoot:content-type*) "application/json")

      (if (verify-policy-and-create-document conditions channel)
          (let* ((policy-as-json (st-json:write-json-to-string data))
                 (policy-encoded (base64:usb8-array-to-base64-string (babel:string-to-octets policy-as-json :encoding :utf-8)))
                 (signature (compute-s3-upload-policy-signature policy-encoded)))
            (st-json:jso "policy" policy-encoded
                         "signature" signature))
          ;; ELSE: Illegal policy
          (progn
            (log:warn "Illegal policy, channel-id=~s, user=~s" channel-id (potato.core:user/id (potato.core:current-user)))
            (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
            (st-json:jso "invalid" :true))))))

(defun send-file-upload-message-to-channel (file)
  (let* ((channel (potato.db:load-instance 'potato.core:channel (file/channel file)))
         (message (potato.core:make-message (potato.core:channel/id channel) (file/user file) (file/message file)
                                           :files (list (list (file/id file) (file/name file) (file/size file))))))
    (potato.core:save-message message)
    ;; Send a message to the image processor so that it
    ;; can be updated with an image thumbnail if
    ;; applicable.
    (with-pooled-rabbitmq-connection (conn)
      (cl-rabbit:basic-publish conn 1
                               :exchange *chat-image-converter-acceptor-exchange-name*
                               :body (lisp-to-binary (list (file/id file) (potato.core:message/id message)))))
    message))

(potato.core:define-handler-fn-login (upload-success-result-screen "/s3/success" nil ())
  (check-s3-active)
  (potato.core:with-authenticated-user ()
    (let ((params (hunchentoot:post-parameters*)))
      (log:trace "Upload success. params=~s" params)
      (lofn:with-parameters ((key "key"))
        (let ((file (potato.db:load-instance 'file (make-file-id key))))
          (unless (file/verify-date file)
            (error "File has not been verified"))
          (when (file/confirmed-p file)
            (error "Attempt to confirm existing file: ~s" key))
          (unless (string= (file/user file) (potato.core:user/id (potato.core:current-user)))
            (error "Confirmation from the wrong user. id=~s, current user=~s"
                   (file/key file) (potato.core:user/id (potato.core:current-user))))
          (multiple-value-bind (data code response)
              (zs3:head :bucket *s3-bucket* :key (file/key file))
            (unless (= code hunchentoot:+http-ok+)
              (error "Unable to check file size on S3, code=~s, message=~s" code response))
            (let ((size (parse-integer (getfield :content-length data))))
              (log:trace "Successful upload: ~s" file)
              (setf (file/confirmed-p file) (local-time:now))
              (setf (file/size file) size)
              (potato.db:save-instance file)
              (send-file-upload-message-to-channel file)
              (st-json:write-json-to-string (st-json:jso "file" (file/name file))))))))))

(defun create-file-s3 (user channel filename mime-type input-file subdirectory)
  (let ((user (potato.core:ensure-user user))
        (channel (potato.core:ensure-channel channel)))
    (multiple-value-bind (match strings)
        (cl-ppcre:scan-to-strings ".+(\\.[^.]+)$" filename)
      (let* ((cid (potato.core:channel/id channel))
             (key (format nil "~a/~a/~@[~a/~]~a~a"
                          *s3-directory*
                          cid
                          subdirectory
                          (make-random-name 40)
                          (if match (aref strings 0) "")))
             (file (make-instance 'file
                                  :name filename
                                  :channel cid
                                  :key key
                                  :user (potato.core:user/id user)
                                  :message "Uploaded file"
                                  :mime-type mime-type
                                  :location :s3)))
        (potato.db:save-instance file)
        (zs3:put-file input-file *s3-bucket* key :content-type mime-type)
        (let ((result (zs3:head :bucket *s3-bucket* :key key))
              (now (local-time:now)))
          (setf (file/size file) (parse-integer (getfield :content-length result)))
          (setf (file/verify-date file) now)
          (setf (file/confirmed-p file) now)
          (potato.db:save-instance file)
          file)))))

;;;
;;;  Support for downloads from S3
;;;

(defun encode-rfc5987 (name)
  "Encode a string using RFC 5987 - Character Set and Language
Encoding for Hypertext Transfer Protocol (HTTP) Header Field
Parameters"
  (with-output-to-string (s)
    (write-string "UTF-8''" s)
    (loop
       for ch across (babel:string-to-octets name :encoding :utf-8)
       if (< ch 128)
       do (write-char (code-char ch) s)
       else
       do (format s "%~16,2,'0r" ch))))

(defun find-vhost ()
  "Find the :VHOST parameter for the given endpoint.
This is a bit hackish, and is only needed because ZS3 has some hardcoded
values for Amazon."
  (if (s3-wasabi-enabled)
      :wasabi
      :amazon))

(defun download-s3 (file)
  (check-s3-active)
  (let ((url (zs3-authorized-url :bucket *s3-bucket*
                                 :key (file/key file)
                                 :expires (+ (get-universal-time) 60)
                                 :vhost :amazon
                                 :content-disposition (format nil "inline; filename*=~a" (encode-rfc5987 (file/name file)))
                                 :content-type (or (file/mime-type file) "binary/octet-stream")
                                 :ssl t)))
    (log:trace "Redirecting download to URL: ~a" url)
    (hunchentoot:redirect url)))
