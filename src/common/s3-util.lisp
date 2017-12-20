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
       (setf zs3::*permanent-redirects* hash)
       (when (s3-wasabi-enabled)
         (setq zs3:*s3-endpoint* "s3.wasabisys.com"))))))

(defun s3-wasabi-enabled ()
  (cl-ppcre:scan "^https?://s3.wasabisys.com/" *s3-endpoint*))

(defun zs3-authorized-url (&key bucket key vhost expires ssl sub-resource content-disposition content-type
                         ((:credentials zs3:*credentials*) zs3:*credentials*))
  (unless (and expires (integerp expires) (plusp expires))
    (error "~S option must be a positive integer" :expires))
  (let* ((region (zs3::bucket-region bucket))
         (region-endpoint (zs3::region-endpoint region))
         (endpoint (case vhost
                     (:cname bucket)
                     (:amazon (format nil "~A.~A" bucket region-endpoint))
                     (:wasabi (format nil "~a.s3.wasabisys.com" bucket))
                     ((nil) region-endpoint)))
         (extra-parameters (append (if content-disposition
                                       (list (cons "response-content-disposition" content-disposition)))
                                   (if content-type
                                       (list (cons "response-content-type" content-type)))))
         (request (make-instance 'zs3::url-based-request
                                 :method :get
                                 :bucket bucket
                                 :region region
                                 :endpoint endpoint
                                 :sub-resource sub-resource
                                 :key key
                                 :expires (zs3::unix-time expires)
                                 :parameters extra-parameters)))
    (setf (zs3::amz-headers request) nil)
    (setf (zs3::parameters request)
          (zs3:parameters-alist "X-Amz-Algorithm" "AWS4-HMAC-SHA256"
                            "X-Amz-Credential"
                            (format nil "~A/~A/~A/s3/aws4_request"
                                    (zs3:access-key zs3:*credentials*)
                                    (zs3::iso8601-basic-date-string (zs3::date request))
                                    (zs3::region request))
                            "X-Amz-Date" (zs3::iso8601-basic-timestamp-string (zs3::date request))
                            "X-Amz-Expires" (- expires (get-universal-time))
                            "X-Amz-SignedHeaders"
                            (format nil "~{~A~^;~}" (zs3::signed-headers request))))
    (setf (zs3::parameters request) (append (zs3::parameters request) extra-parameters))
    (push (cons "X-Amz-Signature" (zs3::request-signature request))
          (zs3::parameters request))
    (let ((parameters (zs3::alist-to-url-encoded-string (zs3::parameters request))))
      (case vhost
        (:cname
         (format nil "http~@[s~*~]://~A/~@[~A~]?~@[~A&~]~A"
                 ssl
                 bucket
                 (zs3::url-encode key :encode-slash nil)
                 sub-resource
                 parameters))
        (:amazon
         (format nil "http~@[s~*~]://~A/~@[~A~]?~@[~A&~]~A"
                 ssl
                 endpoint
                 (zs3::url-encode key :encode-slash nil)
                 sub-resource
                 parameters))
        (:wasabi
         (format nil "http~@[s~*~]://~A/~@[~A~]?~@[~A&~]~A"
                 ssl
                 endpoint
                 (zs3::url-encode key :encode-slash nil)
                 sub-resource
                 parameters))
        ((nil)
         (format nil "http~@[s~*~]://~A/~@[~A/~]~@[~A~]?~@[~A&~]~A"
                 ssl
                 endpoint
                 (zs3::url-encode bucket)
                 (zs3::url-encode key :encode-slash nil)
                 sub-resource
                 parameters))))))

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
