(in-package :potato-client-clim)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defclass image-cache-entry ()
  ((src       :type (or user string)
              :initarg :src
              :reader image-cache-entry/src)
   (pixmap    :initform nil
              :accessor image-cache-entry/pixmap)
   (callbacks :type list
              :initform nil
              :accessor image-cache-entry/callbacks)
   (loading-p :initform t
              :accessor image-cache-entry/loading-p)))

(defclass image-cache ()
  ((images     :initform (make-instance 'receptacle:hash-map :test 'equal)
               :reader image-cache/images)
   (lock       :initform (bordeaux-threads:make-lock "Image cache lock")
               :reader image-cache/lock)
   (connection :type potato-client:connection
               :initarg :connection
               :reader image-cache/connection)))

(defgeneric load-image-from-src (src stream cache))

(defgeneric make-image-cache-key (src))

(defun suffix-from-type (type)
  (string-case:string-case (type)
    ("image/png" "png")
    ("image/jpeg" "jpg")))

(defun load-image-in-cache (cache entry)
  (check-type cache image-cache)
  (check-type entry image-cache-entry)
  (labels ((notify-callbacks ()
             (let ((callbacks (image-cache-entry/callbacks entry)))
               (setf (image-cache-entry/callbacks entry) nil)
               (dolist (callback callbacks)
                 (funcall callback entry nil)))))
    (let (type)
      (let ((data (flexi-streams:with-output-to-sequence (seq-out)
                    (setq type (load-image-from-src (image-cache-entry/src entry) seq-out cache)))))
        (if type
            (uiop:with-temporary-file (:stream stream :pathname file :type (suffix-from-type type))
              (write-sequence data stream)
              :close-stream
              (let ((pattern (clim:make-pattern-from-bitmap-file file)))
                (setf (image-cache-entry/pixmap entry) pattern)
                (setf (image-cache-entry/loading-p entry) nil)
                (notify-callbacks)))
            ;; ELSE: No image found
            (progn
              (setf (image-cache-entry/loading-p entry) nil)
              (notify-callbacks)))))))

(defmethod make-image-cache-key ((src string))
  (list :url src))

(defmethod load-image-from-src ((url string) stream cache)
  (multiple-value-bind (content code headers uri remote-stream should-close reason)
      (drakma:http-request url
                           :want-stream t
                           :force-binary t)
    (declare (ignore content uri reason))
    (unwind-protect
         (progn
           (unless (= code 200)
             (error "Failed to load image"))
           (uiop:copy-stream-to-stream remote-stream stream :element-type '(unsigned-byte 8))
           (cdr (assoc :content-type headers)))
      (when should-close
        (close remote-stream)))))

(defun find-image-from-url (cache src callback)
  (let ((found (bordeaux-threads:with-lock-held ((image-cache/lock cache))
                 (let* ((values (image-cache/images cache))
                        (key (make-image-cache-key src))
                        (entry (receptacle:hash-get values key)))
                   (cond ((null entry)
                          (let ((e (make-instance 'image-cache-entry :src src)))
                            (push callback (image-cache-entry/callbacks e))
                            (setf (receptacle:hash-get values key) e)
                            (lparallel:future
                              (load-image-in-cache cache e))
                            nil))
                         ((image-cache-entry/loading-p entry)
                          (push callback (image-cache-entry/callbacks entry))
                          nil)
                         (t
                          entry))))))
    (when found
      (funcall callback found t))))
