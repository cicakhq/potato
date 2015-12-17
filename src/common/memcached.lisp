(in-package :potato.common.memcached)

(declaim #.potato.common::*compile-decl*)

(defvar *memcached-hostname* "127.0.0.1"
  "The hostname of the memcached server")

(defvar *memcached-port* 11211
  "The port number of the memcached server")

(potato.common.application:define-component memcached
  (:start
   (setq cl-memcached:*mc-use-pool* t)
   (setq cl-memcached:*memcache* (cl-memcached:make-memcache :ip *memcached-hostname* :port *memcached-port*))
   ;; The call to MC-VERSION will raise an exception if memcached is not available
   (cl-memcached:mc-version)))

(defun find-cached-object-if-exists (memcached-key)
  (let ((cached (cl-memcached:mc-get (list memcached-key))))
    (when cached
      (let ((state (potato.common:decode-conspack-with-interning (fifth (car cached)))))
        state))))
