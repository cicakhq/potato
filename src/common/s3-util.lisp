(in-package :potato.common)

(declaim #.potato.common::*compile-decl*)

(defvar *s3-bucket* nil
  "The name of the S3 bucket")
(defvar *s3-directory* nil
  "The directory name in which all files will be uploaded")
(defvar *s3-secret-key* nil
  "S3 secret key")
(defvar *s3-access-key* nil
  "S3 access key")
(defvar *s3-browser-secret-key* nil
  "S3 secret key for the client")
(defvar *s3-browser-access-key* nil
  "S3 access key for the client")
(defvar *s3-endpoint* nil
  "S3 endpoint hostname")

(defun s3-enabled-p ()
  (and *s3-bucket* *s3-directory*
       *s3-access-key* *s3-secret-key*
       *s3-browser-access-key* *s3-browser-secret-key*
       *s3-endpoint*))

(defun check-s3-active ()
  (unless (s3-enabled-p)
    (error "S3 has not been configured")))

(defclass potato-s3-credentials ()
  ()
  (:documentation "Hardcoded S3 credentials for potato"))

(defmethod zs3:access-key ((credentials potato-s3-credentials))
  (declare (ignore credentials))
  (unless *s3-access-key*
    (error "*s3-access-key* not configured"))
  *s3-access-key*)

(defmethod zs3:secret-key ((credentials potato-s3-credentials))
  (declare (ignore credentials))
  (unless *s3-secret-key*
    (error "*s3-secret-key* not configured"))
  *s3-secret-key*)

(potato.common.application:define-component s3
  (:start
   (setq zs3:*credentials* (make-instance 'potato-s3-credentials))
   (when (s3-enabled-p)
     (let ((hash (make-hash-table :test #'equalp)))
       (setf (gethash '("s3.amazonaws.com" *s3-bucket* *s3-access-key*) hash) "the-potato.s3.amazonaws.com")
       (setf zs3::*permanent-redirects* hash)))))

(defun zs3-authorized-url (&key bucket key vhost expires ssl sub-resource content-disposition content-type
                             ((:credentials zs3:*credentials*) zs3:*credentials*))
  (unless (and expires (integerp expires) (plusp expires))
    (error "~S option must be a positive integer" :expires))

  (let* ((extra-parameters (append (if content-disposition
                                       (list (cons "response-content-disposition" content-disposition)))
                                   (if content-type
                                       (list (cons "response-content-type" content-type)))))

         (request (make-instance 'zs3::url-based-request
                                 :method :get
                                 :bucket bucket
                                 :sub-resource sub-resource
                                 :key key
                                 :expires (zs3::unix-time expires)
                                 :parameters extra-parameters))

         (parameters
           (zs3::alist-to-url-encoded-string
            (list* (cons "AWSAccessKeyId" (zs3:access-key zs3:*credentials*))
                   (cons "Expires" (format nil "~D" (zs3::expires request)))
                   (cons "Signature" (zs3::signature request))
                   extra-parameters))))
    (log:trace "Creating authorised url. key=~s, parameters=~s" key parameters)
    (case vhost
      (:cname
       (format nil "http~@[s~*~]://~A/~@[~A~]?~@[~A&~]~A"
               ssl bucket (zs3::url-encode key) sub-resource parameters))
      (:amazon
       (format nil "http~@[s~*~]://~A.s3.amazonaws.com/~@[~A~]?~@[~A&~]~A"
               ssl bucket (zs3::url-encode key) sub-resource parameters))
      (:google
       (format nil "http~@[s~*~]://~A.storage.googleapis.com/~@[~A~]?~@[~A&~]~A"
               ssl bucket (zs3::url-encode key) sub-resource parameters))
      ((nil)
       (format nil "http~@[s~*~]://s3.amazonaws.com/~@[~A/~]~@[~A~]?~@[~A&~]~A"
               ssl (zs3::url-encode bucket) (zs3::url-encode key) sub-resource
                   parameters)))))

(defmethod zs3::signed-path ((request zs3::request))
  (let ((*print-pretty* nil))
    (with-output-to-string (stream)
      (write-char #\/ stream)
      (when (zs3::bucket request)
        (write-string (zs3::url-encode (zs3::name (zs3::bucket request))) stream)
        (write-char #\/ stream))
      (when (zs3::key request)
        (write-string (zs3::url-encode (zs3::name (zs3::key request))) stream))
      (when (or (zs3::parameters request) (zs3::sub-resource request))
        (write-string "?" stream)
        (if (zs3::parameters request)
            (loop
               for param in (zs3::parameters request)
               for first = t then nil
               unless first
               do (write-string "&" stream)
               do (progn
                    (write-string (zs3::url-encode (car param)) stream)
                    (write-string "=" stream)
                    (write-string (cdr param) stream)))
            (write-string (zs3::url-encode (zs3::sub-resource request)) stream))))))

(defmethod zs3::signed-path :around ((request zs3::request))
  (let ((result (call-next-method)))
    (log:trace "Signed URL: ~a" result)
    result))
