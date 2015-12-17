(in-package :potato.user-image)

(declaim #.potato.common::*compile-decl*)

(defun make-user-image-memcached-key (user)
  (concatenate 'string "image-" (ensure-user-id user)))

#+nil(defun save-instance-force (obj)
  (handler-case
      (potato.db:save-instance obj)
    (clouchdb:id-or-revision-conflict ()
      (let ((loaded (potato.db:load-instance (class-of obj) (potato.db:persisted-entry/couchdb-id obj))))
        (setf (potato.db:persisted-entry/couchdb-revision obj) (potato.db:persisted-entry/couchdb-revision loaded))
        (potato.db:save-instance obj)))))

(defgeneric generate-new-random-image-name (user)
  (:method ((user string))
    (generate-new-random-image-name (load-user user)))
  (:method ((user user))
    (let ((name (make-random-name 20)))
      (potato.db:call-clouchdb-update-function "user" "update_image_name" (user/id user) `(("name" . ,name)))
      (setf (user/image-name user) name)
      user)))

(defgeneric user-save-image (user image-data)
  (:method ((user string) image-data)
    (user-save-image (load-user user) image-data))
  (:method ((user user) image-data)
    (generate-new-random-image-name user)
    (potato.db:save-attachment user "image" image-data :content-type "image/jpeg")
    (cl-memcached:mc-del (make-user-image-memcached-key user))))

(defun %load-user-image-maybe-empty (user)
  (handler-case
      (let ((stream (potato.db:load-attachment user "image")))
        (unwind-protect
             (flexi-streams:with-output-to-sequence (output)
               (uiop/stream:copy-stream-to-stream stream output :element-type '(unsigned-byte 8)))
          (close stream)))
    ;; No attachment, return a blank image
    (clouchdb:attachment-missing () nil)))

(defgeneric user-load-image (user)
  (:method ((user string))
    (user-load-image (load-user user)))
  (:method ((user user))
    (let* ((key (make-user-image-memcached-key user))
           (cached (cl-memcached:mc-get (list key))))
      (if cached
          (fifth (car cached))
          (let ((cached-data (%load-user-image-maybe-empty user)))
            (cl-memcached:mc-set key cached-data)
            cached-data)))))

(defgeneric image-url-for-user (user)
  (:method ((user string))
    (image-url-for-user (load-user user)))
  (:method ((user user))
    (let* ((image-name (user/image-name user)))
      (if (string= image-name "")
          (format nil "/assets/img/users/~a" (user/default-image-name user))
          (format nil "/user_image/~a/~a" (user/id user) image-name)))))

(define-handler-fn-login (user-image-screen "/user_image/([^/]+)/([A-Za-z0-9]+)" t (user-id name))
  (log:trace "Getting image for user-id=~s, name=~s" user-id name)
  (let* ((user (load-user user-id))
         (image-name (user/image-name user)))
    (cond ((string/= image-name name)
           (hunchentoot:redirect (image-url-for-user user)))
          (t
           (let ((content (user-load-image user)))
             (unless content
               (error "User has image, not no content found."))
             (setf (hunchentoot:content-type*) "image/png")
             (setf (hunchentoot:header-out "cache-control") "public,max-age=86400")
             content)))))

(define-handler-fn-login (upload-image-screen "/upload_image" nil ())
  (with-authenticated-user ()
    (let ((photo-params (hunchentoot:post-parameter "photo")))
      (unless photo-params
        (error "No file was uploaded"))
      (potato.image-convert:convert-user-image (first photo-params) (user/id (current-user)))
      (hunchentoot:redirect "/settings"))))
