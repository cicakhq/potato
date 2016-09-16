(in-package :potato-client-clim)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defclass user ()
  ((id          :type string
                :initarg :id
                :reader user/id)
   (description :type string
                :initarg :description
                :accessor user/description)
   (nickname    :type string
                :initarg :nickname
                :accessor user/nickname)))

(defmethod print-object ((obj user) stream)
  (print-unreadable-safely (id description) obj stream
    (format stream "ID ~s NAME ~s" id description)))

(defclass user-db ()
  ((users       :type hash-table
                :initform (make-hash-table :test 'equal)
                :reader user-db/users)
   (lock        :type t
                :initform (bordeaux-threads:make-lock "User database lock")
                :reader user-db/lock)
   (callback-fn :type (or null function)
                :initarg :callback-fn
                :initform nil
                :reader user-db/callback-fn)))

(defun find-user (user-db uid)
  (check-type user-db user-db)
  (check-type uid string)
  (bordeaux-threads:with-lock-held ((user-db/lock user-db))
    (let ((user (gethash uid (user-db/users user-db))))
      (cond ((null user)
             (let ((u (make-instance 'user
                                     :id uid
                                     :description "empty"
                                     :nickname "empty")))
               (setf (gethash uid (user-db/users user-db)) u)
               (log:warn "Currently not updating the user name")
               u))
            (t
             user)))))

(defun update-user (user-db uid description nickname)
  (let* ((user (gethash uid (user-db/users user-db))))
    (cond (user
           (setf (user/description user) description)
           (setf (user/nickname user) nickname)
           user)
          (t
           (setf (gethash uid (user-db/users user-db))
                 (make-instance 'user :id uid :description description :nickname nickname))))))

(defun update-users-from-channel (user-db conn cid)
  (check-type user-db user-db)
  (check-type cid string)
  (let ((res (potato-client:list-users cid :connection conn)))
    (bordeaux-threads:with-lock-held ((user-db/lock user-db))
      (let ((updated (loop
                       for user-data in res
                       collect (update-user user-db
                                            (cdr (assoc :id user-data))
                                            (cdr (assoc :description user-data))
                                            (cdr (assoc :nickname user-data))))))
        (alexandria:when-let ((callback (user-db/callback-fn user-db)))
          (funcall callback updated))))))

(defun users-in-db (user-db)
  (check-type user-db user-db)
  (sort (loop
          for ch being each hash-value in (user-db/users user-db)
          collect ch)
        #'string< :key #'user/description))

(defmethod load-image-from-src ((user user) stream cache)
  (potato-client:user-image (user/id user) stream :connection (image-cache/connection cache))
  "image/png")

(defmethod make-image-cache-key ((user user))
  (list :user (user/id user)))
