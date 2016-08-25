(in-package :potato-client-clim)

(defclass user ()
  ((id                :type string
                      :initarg :id
                      :reader user/id)
   (description       :type string
                      :initarg :description
                      :accessor user/description)
   (nickname          :type string
                      :initarg :nickname
                      :accessor user/nickname)
   (waiting-callbacks :type list
                      :initarg :waiting-callbacks
                      :initform nil
                      :accessor user/waiting-callbacks)
   (sync-active       :type (or null (eql t))
                      :initarg :sync-active
                      :initform nil
                      :accessor user/sync-active)))

(defclass user-db ()
  ((users      :type hash-table
               :initform (make-hash-table :test 'equal)
               :reader user-db/users)
   (connection :type potato-client:connection
               :initarg :connection
               :reader user-db/connection)
   (lock       :type t
               :initform (bordeaux-threads:make-lock "User database lock")
               :reader user-db/lock)))

(defun find-user (user-db uid &optional callback-fn)
  (check-type user-db user-db)
  (check-type uid string)
  (check-type callback-fn (or null function))
  (bordeaux-threads:with-lock-held ((user-db/lock user-db))
    (let ((user (gethash uid (user-db/users user-db))))
      (cond ((null user)
             (let ((u (make-instance 'user
                                     :id uid
                                     :description "empty"
                                     :nickname "empty"
                                     :waiting-callbacks (if callback-fn (list callback-fn) nil)
                                     :sync-active t)))
               (setf (gethash uid (user-db/users user-db)) u)
               (log:warn "Currently not updating the user name")
               u))
            ((user/sync-active user)
             (when callback-fn
               (push callback-fn (user/waiting-callbacks user)))
             user)
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

(defun update-users-from-channel (user-db cid)
  (check-type user-db user-db)
  (check-type cid string)
  (let ((res (potato-client:list-users cid :connection (user-db/connection user-db))))
    (bordeaux-threads:with-lock-held ((user-db/lock user-db))
      (loop
        for user-data in res
        collect (update-user user-db
                        (cdr (assoc :id user-data))
                        (cdr (assoc :description user-data))
                        (cdr (assoc :nickname user-data)))))))
