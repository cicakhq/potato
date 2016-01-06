;;;
;;;  This file deals with the concept of "display config". The display
;;;  config is all the options that can change during the use of the
;;;  web application. For example, the list of channels that the user
;;;  wants to hide from the sidebar is part of this object.
;;;

(in-package :potato.core)

(declaim #.potato.common::*compile-decl*)

(defclass display-config (potato.db:persisted-entry)
  ((user     :type string
             :initarg :user
             :reader display-config/user
             :persisted-p t)
   (timezone :type string
             :initarg :timezone
             :initform "UTC"
             :accessor display-config/timezone
             :persisted-p t
             :persisted-allow-missing-value t
             :persisted-missing-default "UTC"))
  (:metaclass potato.db:persisted-entry-class)
  (:attachments-p nil)
  (:memcached-enabled-p t)
  (:documentation "User interface personalisation data"))

(defmethod print-object ((obj display-config) stream)
  (print-unreadable-safely (user) obj stream
    (format stream "~s" user)))

(defun make-user-display-config-id (user)
  (format nil "userdisplayconfig-~a" (ensure-user-id user)))

(defmethod initialize-instance :after ((obj display-config) &key)
  (let ((id (make-user-display-config-id (display-config/user obj))))
    (cond ((null (potato.db:persisted-entry/couchdb-id obj))
           (setf (potato.db:persisted-entry/couchdb-id obj) id))
          ((string/= (potato.db:persisted-entry/couchdb-id obj) id)
           (error "Display config ID does not match user. id=~s, user=~s"
                  (potato.db:persisted-entry/couchdb-id obj) (display-config/user obj))))))

(defun load-display-config-for-user (user)
  (or (potato.db:load-instance 'display-config (make-user-display-config-id user) :error-if-not-found nil)
      (let ((config (make-instance 'display-config
                                   :user (ensure-user-id user))))
        (potato.db:save-instance config)
        config)))

(defun format-timestamp-for-display-config (display-config ts)
  (let ((tz (local-time:find-timezone-by-location-name (display-config/timezone display-config))))
    (local-time:format-timestring nil ts
                                  :format '(:short-weekday " " :day " "
                                            :short-month " " :year ", "
                                            (:hour 2) ":" (:min 2) " " :timezone)
                                  :timezone tz)))
