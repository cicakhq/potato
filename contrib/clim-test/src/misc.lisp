(defpackage :clim-test
  (:use :cl))

(defvar *job-handler* nil)
(defvar *job-handler-queue* (dhs-sequences:make-blocking-queue :name "Job handler queue"))

(defclass channel ()
  ((id      :type string
            :initarg :id
            :reader channel/id)
   (name    :type string
            :initarg :name
            :reader channel/name)
   (domain  :type string
            :initarg :domain
            :reader channel/domain)
   (group   :type string
            :initarg :group
            :reader channel/group)
   (private :type t
            :initarg :private
            :reader channel/private)))

(defmethod print-object ((obj channel) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "ID ~s NAME ~s PRIVATE ~s"
            (slot-value obj 'id)
            (slot-value obj 'name)
            (slot-value obj 'private))))

(defclass connection-state ()
  ((api-key    :type string
               :initform (error "~s not specified when creating new ~s" :api-key 'connection-state)
               :initarg :api-key
               :reader connection-state/api-key)
   (url-prefix :type string
               :initform "https://potato.dhsdevelopments.com/"
               :initarg :url-prefix
               :reader connection-state/url-prefix)
   (channels   :type list
               :initform nil
               :accessor connection-state/channels)))

;;;
;;;  job handlers
;;;

(defclass job ()
  ((callback :type function
             :initarg :callback
             :reader job/callback)))

(defun job-handler-loop ()
  (loop
    for job = (dhs-sequences:queue-pop-wait *job-handler-queue*)
    do (restart-case
            (funcall (job/callback job))
         (skip-current-job ()
           :report "Skip current job"
           nil))))

(defun start-job-handler ()
  (if *job-handler*
      (log:warn "Job handler already running, not starting")
      (let ((th (bordeaux-threads:make-thread #'job-handler-loop :name "Job handler thread")))
        (setq *job-handler* th))))

(defun submit-new-job (callback)
  (let ((job (make-instance 'job :callback callback)))
    (dhs-sequences:queue-push *job-handler-queue* job)))

(defmacro with-submitted-job ((conn) &body body)
  (let ((conn-sym (gensym "CONN-")))
    `(let ((,conn-sym ,conn))
       (check-type ,conn-sym connection-state)
       (submit-new-job (lambda () ,@body)))))

(defun potato-request (conn url)
  (let* ((url (format nil "~aapi/1.0~a" (connection-state/url-prefix conn) url)))
    (multiple-value-bind (body code headers origin-url stream should-close)
        (drakma:http-request url
                             :additional-headers `(("API-Token" . ,(connection-state/api-key conn)))
                             :force-binary t
                             :want-stream t)
      (declare (ignore body headers origin-url))
      (unwind-protect
           (let* ((s (flexi-streams:make-flexi-stream stream :external-format :utf-8))
                  (result (st-json:read-json s)))
             (if (= code 200)
                 result
                 (error "Error when reading result from potato server: ~s" result)))
        (when should-close
          (close stream))))))
