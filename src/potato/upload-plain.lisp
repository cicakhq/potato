(in-package :potato.upload)

(declaim #.potato.common::*compile-decl*)

;;;
;;;  Support for local file uploads
;;;

(defvar *file-upload-directory* nil
  "The root directory for uploads")

(defvar *default-upload-location* nil
  "Default upload location, or NIL if uploads are disabled")

(defvar *file-upload-manager* nil
  "Provider for local file uploads")

(defgeneric upload-manager/copy-stream-to-storage (upload-manager input-stream file)
  (:documentation "Read the entirety of INPUT-STREAM to backend
storage and update the FILE instance to indicate that the file has
been saved."))

(defgeneric upload-manager/create-file (upload-manager user channel filename mime-type subdirectory)
  (:documentation "Returns the key for the given file"))

(defgeneric upload-manager/copy-storage-to-stream (upload-manager file stream)
  (:documentation "Write the content of FILE to STREAM"))

(defun check-local-upload-active ()
  (unless *file-upload-manager*
    (error "Local uploads are not enabled")))

(defun process-multipart-upload (upload-manager cid filename content-type uploaded-file)
  (ecase *default-upload-location*
    (:s3
     (let ((file (create-file-s3 (potato.core:current-user) cid filename content-type uploaded-file nil)))
       (send-file-upload-message-to-channel file)))
    (:file
     (let* ((channel (potato.core:load-channel-with-check cid))
            (file (upload-manager/create-file upload-manager (potato.core:current-user) channel filename nil)))
       (setf (file/mime-type file) content-type)
       (potato.db:save-instance file)
       (with-open-file (stream uploaded-file :direction :input :element-type '(unsigned-byte 8))
         (upload-manager/copy-stream-to-storage upload-manager stream file))
       (uiop:delete-file-if-exists uploaded-file)
       (send-file-upload-message-to-channel file)))))

(defun upload-file (upload-manager)
  (let ((filename (hunchentoot:post-parameter "qqfilename"))        
        (cid (hunchentoot:post-parameter "channel")))
    (destructuring-bind (uploaded-file uploaded-name uploaded-content-type)
        (hunchentoot:post-parameter "qqfile")
      (log:trace "Uploading file. filename=~s, mime-type=~s" uploaded-name uploaded-content-type)
      (process-multipart-upload upload-manager cid filename uploaded-content-type uploaded-file)
      (lofn:with-hunchentoot-stream (stream "application/json")
        (st-json:write-json (st-json:jso "success" (st-json:as-json-bool t)) stream)))))

(potato.core:define-handler-fn-login (upload-plain-screen "/upload" nil ())
  (check-local-upload-active)
  (potato.core:with-authenticated-user ()
    (lofn:case-method
      (:post (upload-file *file-upload-manager*)))))

;;;
;;;  Plain file storage
;;;

(defclass directory-upload-manager ()
  ((path :type pathname
         :initarg :path
         :reader directory-upload-manager/path
         :documentation "Root directory of the file storage")))

(defmethod upload-manager/create-file ((upload-manager directory-upload-manager) user channel filename mime-type subdirectory)
  (let ((user (potato.core:ensure-user user))
        (channel (potato.core:ensure-channel channel)))
    (multiple-value-bind (match strings)
        (cl-ppcre:scan-to-strings ".+(\\.[^.]+)$" filename)
      (let* ((cid (potato.core:channel/id channel))
             (key (format nil "~a/~a/~@[~a/~]~a~a"
                          (potato.core:channel/domain channel)
                          cid
                          subdirectory
                          (make-random-name 40)
                          (if match (aref strings 0) ""))))
        (let ((file (make-instance 'file
                                   :name filename
                                   :channel cid
                                   :key key
                                   :user (potato.core:user/id user)
                                   :message "Uploaded file"
                                   :mime-type mime-type
                                   :location :file)))
          (log:trace "Created file: ~s" file)
          file)))))

(defun copy-stream-to-file (input output-file)
  (with-open-file (output-stream output-file :direction :output :if-exists :error :element-type '(unsigned-byte 8))
    (loop
       with buffer-size = 8192
       with buffer = (make-array (list buffer-size) :element-type '(unsigned-byte 8))
       for end = (read-sequence buffer input)
       until (zerop end)
       do (write-sequence buffer output-stream :end end)
       summing end)))

(defmethod upload-manager/copy-stream-to-storage ((upload-manager directory-upload-manager) stream file)
  (let ((output-file (merge-pathnames (file/key file) (directory-upload-manager/path upload-manager))))
    (uiop:ensure-all-directories-exist (list output-file))
    (let ((length (copy-stream-to-file stream output-file)))
      (let ((now (local-time:now)))
        (setf (file/verify-date file) now)
        (setf (file/confirmed-p file) now)
        (setf (file/size file) length)
        (potato.db:save-instance file)))))

(defmethod upload-manager/copy-storage-to-stream ((upload-manager directory-upload-manager) key stream)
  (let ((filename (merge-pathnames key (directory-upload-manager/path upload-manager))))
    (with-open-file (in filename :element-type '(unsigned-byte 8))
      (uiop:copy-stream-to-stream in stream :element-type '(unsigned-byte 8)))))

;;;
;;;  Support for downloads
;;;

(defun download-plain (file)
  (setf (hunchentoot:content-type*) (file/mime-type file))
  (setf (hunchentoot:content-length*) (file/size file))
  (setf (hunchentoot:header-out "Content-Disposition")
        (format nil "inline; filename*=~a" (encode-rfc5987 (file/name file))))
  (let ((out (hunchentoot:send-headers)))
    (upload-manager/copy-storage-to-stream *file-upload-manager* (file/key file) out)))

(defun make-download-location (file-id)
  (format nil "/download/~a" (url-rewrite:url-encode file-id)))

(defun download-file-to-client (file)
  (ecase (file/location file)
    (:s3 (download-s3 file))
    (:file (download-plain file))))

(potato.core:define-handler-fn-login (download-screen "/download/([^/]+)" t (key))
  (potato.core:with-authenticated-user ()
    (log:trace "Downloading file: ~s" key)
    (let ((file (potato.db:load-instance 'file key)))
      (unless (file/confirmed-p file)
        (error "File does not exist"))
      (potato.core:check-user-in-channel (file/channel file))
      (download-file-to-client file))))

;;;
;;;  Direct access to file content
;;;

(defun download-file-by-filesource (file-source destination)
  (let ((file (potato.db:load-instance 'file file-source)))
    (ecase (file/location file)
      (:s3 (zs3:get-file *s3-bucket* (file/key file) destination))
      (:file (with-open-file (s destination :direction :output
                                            :element-type '(unsigned-byte 8)
                                            :if-does-not-exist :error
                                            :if-exists :supersede)
               (upload-manager/copy-storage-to-stream *file-upload-manager* (file/key file) s))))))

(defun upload-file-by-name (input-file destination-name mime-type message subdirectory)
  (let ((message (potato.core:ensure-message message)))
    (ecase *default-upload-location*
      (:s3
       (create-file-s3 (potato.core:message/from message)
                       (potato.core:message/channel message)
                       destination-name
                       mime-type
                       input-file
                       subdirectory))
      (:file
       (let ((file (upload-manager/create-file *file-upload-manager*
                                               (potato.core:message/from message)
                                               (potato.core:message/channel message)
                                               destination-name
                                               mime-type
                                               subdirectory)))
         (with-open-file (s input-file :direction :input :element-type '(unsigned-byte 8))
           (upload-manager/copy-stream-to-storage *file-upload-manager* s file))
         file)))))

;;;
;;;  Service definition
;;;

(potato.common.application:define-component upload
  (:start
   (when *file-upload-directory*
     (setq *file-upload-manager* (make-instance 'directory-upload-manager :path (pathname *file-upload-directory*))))
   (ecase *default-upload-location*
     (:s3 (unless (s3-enabled-p)
            (error "Default upload location is S3, but S3 is not configured")))
     (:file (unless *file-upload-directory*
              (error "Default upload location is file, but upload location is not configured")))
     ((nil) (cond ((s3-enabled-p)
                   (log:warn "No default upload location configured but S3 available, choosing S3")
                   (setq *default-upload-location* :s3))
                  (*file-upload-directory*
                   (log:warn "No default upload location configured but file available. Choosing file.")
                   (setq *default-upload-location* :file))
                  (t
                   (log:warn "No file upload location configured.")))))))
