(in-package :potato.core)

(declaim #.potato.common::*compile-decl*)

(defclass domain-nickname ()
  ((domain    :type string
              :initarg :domain
              :reader domain-nickname/domain
              :persisted-p t
              :persisted-name :|domain|)
   (nickname  :type string
              :initarg :nickname
              :reader domain-nickname/nickname
              :persisted-p t))
  (:metaclass potato.db:persisted-entry-class)
  (:memcached-enabled-p t))

(defmethod print-object ((obj domain-nickname) stream)
  (print-unreadable-safely (domain nickname) obj stream
    (format stream "DOMAIN ~s NICKNAME ~s" domain nickname)))

(defun make-domain-nickname-couchdb-key (key)
  (format nil "domain-nickname-~a" (encode-name key)))

(defmethod initialize-instance :after ((obj domain-nickname) &key)
  (unless (potato.db:persisted-entry/couchdb-id obj)
    (setf (potato.db:persisted-entry/couchdb-id obj) (make-domain-nickname-couchdb-key (domain-nickname/nickname obj)))))

(defun find-domain-nickname (nickname)
  (potato.db:load-instance 'domain-nickname (make-domain-nickname-couchdb-key nickname)))

(defun find-domain-nickname-from-domain-id (domain-id)
  (let ((nicknames (potato.db:invoke-view-and-load-instances 'domain-nickname "domain" "domain_nickname" :key domain-id)))
    (cond ((null nicknames)
           nil)
          ((null (cdr nicknames))
           (car nicknames))
          (t
           (error "More than one nickname for domain: ~s" domain-id)))))

(defun set-nickname-for-domain (domain nickname)
  "Sets the domain nickname to NICKNAME, or remove it if NICKNAME is NIL."
  (let* ((domain-id (ensure-domain-id domain))
         (dn (potato.core:find-domain-nickname-from-domain-id domain-id)))
    (unless (and dn (equal nickname (potato.core:domain-nickname/nickname dn)))
      (when dn
        (potato.db:remove-instance dn))
      (when nickname
        (let ((domain-nickname (make-instance 'potato.core:domain-nickname :domain domain-id :nickname nickname)))
          (handler-case
              (potato.db:save-instance domain-nickname)
            (clouchdb:id-or-revision-conflict (condition)
              (declare (ignore condition))
              (error "Domain nickname already in use"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default domain management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *domain-config-couchdb-id* "domain-config")

(defclass domain-config ()
  ((root-domain :type (or null string)
                :initarg :root-domain
                :accessor domain-config/root-domain
                :persisted-p t))
  (:metaclass potato.db:persisted-entry-class)
  (:memcached-enabled-p t))

(defmethod initialize-instance :after ((obj domain-config) &key)
  ;; The single instance of this class has a hardcoded id
  (setf (potato.db:persisted-entry/couchdb-id obj) *domain-config-couchdb-id*))

(defun default-domain-id ()
  (let ((conf (potato.db:load-instance 'domain-config *domain-config-couchdb-id* :error-if-not-found nil)))
    (if conf
        (domain-config/root-domain conf)
        nil)))

(defun set-default-domain-id (domain-id)
  (let ((conf (potato.db:load-instance 'domain-config *domain-config-couchdb-id* :error-if-not-found nil)))
    (if conf
        (setf (domain-config/root-domain conf) domain-id)
        (setq conf (make-instance 'domain-config :root-domain domain-id)))
    (potato.db:save-instance conf)))
