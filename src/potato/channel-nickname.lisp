(in-package :potato.core)

(declaim #.potato.common::*compile-decl*)

(defclass channel-nickname ()
  ((domain   :type string
             :initarg :domain
             :reader channel-nickname/domain
             :persisted-p t)
   (channel  :type string
             :initarg :channel
             :reader channel-nickname/channel
             :persisted-p t
             :persisted-name :|channel|)
   (nickname :type string
             :initarg :nickname
             :reader channel-nickname/nickname
             :persisted-p t))
  (:metaclass potato.db:persisted-entry-class)
  (:memcached-enabled-p t))

(defmethod print-object ((obj channel-nickname) stream)
  (print-unreadable-safely (domain channel nickname) obj stream
    (format stream "DOMAIN ~s CHANNEL ~s NICKNAME ~s" domain channel nickname)))

(defun make-channel-nickname-couchdb-key (domain-id key)
  (format nil "channel-nickname-~a-~a" (encode-name domain-id) (encode-name key)))

(defmethod initialize-instance :after ((obj channel-nickname) &key)
  (unless (potato.db:persisted-entry/couchdb-id obj)
    (let ((channel (potato.db:load-instance 'channel (channel-nickname/channel obj))))
      (setf (potato.db:persisted-entry/couchdb-id obj)
            (make-channel-nickname-couchdb-key (channel/domain channel) (channel-nickname/nickname obj))))))

(defun find-channel-nickname (domain nickname)
  (potato.db:load-instance 'channel-nickname (make-channel-nickname-couchdb-key (ensure-domain-id domain) nickname)))

(defun find-channel-nickname-from-channel-id (cid)
  (let ((nicknames (potato.db:invoke-view-and-load-instances 'channel-nickname "channel" "channel_nickname" :key cid)))
    (cond ((null nicknames)
           nil)
          ((null (cdr nicknames))
           (car nicknames))
          (t
           (error "More than one nickname for channel: ~s" cid)))))

(defun find-channel-id-for-nicknames (domain-nickname channel-nickname)
  (alexandria:when-let ((dn (find-domain-nickname domain-nickname)))
    (alexandria:when-let ((cn (find-channel-nickname (domain-nickname/domain dn) channel-nickname)))
      (channel-nickname/channel cn))))

(defun set-nickname-for-channel (channel nickname)
  (let* ((channel (ensure-channel channel))
         (cn (find-channel-nickname-from-channel-id (channel/id channel))))
    (unless (and cn (equal nickname (channel-nickname/nickname cn)))
      (when cn
        (potato.db:remove-instance cn))
      (when nickname
        (let ((channel-nickname (make-instance 'channel-nickname
                                               :domain (channel/domain channel)
                                               :channel (channel/id channel)
                                               :nickname nickname)))
          (handler-case
              (potato.db:save-instance channel-nickname)
            (clouchdb:id-or-revision-conflict (condition)
              (declare (ignore condition))
              (error "Channel nickname already in use"))))))))
