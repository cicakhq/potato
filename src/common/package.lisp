(defpackage :potato.common
  (:use :cl)
  (:documentation "Common functions for potato")
  (:export #:encode-name
           #:decode-name
           #:make-queue
           #:print-unreadable-safely
           #:start-monitored-thread
           #:setup-clouchdb
           #:getfield
           #:*db-name*
           #:*db-port*
           #:*db-hostname* 
           #:*solr-path-location*
           #:init-solr 
           #:generic-init
           #:init-logging
           #:*log-location*
           #:call-with-logged-conditions
           #:value-by-xpath
           #:*debug*
           #:with-temp-file
           #:make-random-name
           #:init-s3
           #:zs3-authorized-url
           #:empty-string-or-nil-p
           #:string-start-match
           #:*rabbitmq-server-name*
           #:*rabbitmq-server-port*
           #:*rabbitmq-user*
           #:*rabbitmq-password*
           #:*unread-state-queue-name*
           #:init-rabbitmq
           #:with-rabbitmq-connected
           #:*unread-state-exchange-name*
           #:+BLANK-CHARS+
           #:with-pooled-rabbitmq-connection
           #:*channel-content-exchange-name*
           #:*user-notifications-exchange-name* 
           #:*state-server-reader-exchange-name*
           #:*state-server-sender-exchange-name*
           #:lisp-to-binary
           #:binary-to-lisp
           #:fsmap-value
           #:fsmap-set
           #:make-fsmap
           #:make-rbtree
           #:rbtree
           #:rbtree-insert
           #:rbtree-remove
           #:map-rbtree
           #:string->sha1
           #:*state-server-message-channel-join*
           #:*state-server-message-channel-join-with-timeout*
           #:*state-server-message-typing-start*
           #:*state-server-message-typing-end*
           #:*state-server-message-sync-users*
           #:*state-server-reply-user-add-to-channel*
           #:*state-server-reply-user-remove-from-channel*
           #:*state-server-reply-typing-start*
           #:*state-server-reply-typing-end*
           #:rbtree-first 
           #:rbtree-remove-first 
           #:*state-server-reply-sync-users*
           #:*chat-image-converter-acceptor-exchange-name*
           #:*chat-image-converter-response-exchange-name*
           #:*chat-image-converter-acceptor-queue-name*
           #:*chat-image-converter-response-queue-name*
           #:markup-from-regexp
           #:escape-string
           #:rbtree-remove-all
           #:ensure-printable-string
           #:current-time
           #:format-message
           #:message-contains-math-p
           #:message-content-as-json
           #:rbtree-size
           #:make-locked-rbtree
           #:*email-queue-name*
           #:*email-exchange-name*
           #:*user-notification-db*
           #:*messages-db*
           #:with-user-notification-db
           #:with-messages-db
           #:with-main-db
           #:recover-if-fail
           #:*message-send-exchange-name*
           #:markup-message-content
           #:*content-processor-queue-name*
           #:nil-if-json-null
           #:*gcm-queue-name*
           #:encode-name-for-routing-key
           #:*channel-exchange-name*
           #:*rabbitmq-vhost*
           #:*rabbitmq-multi-connection-instance*
           #:*s3-bucket*
           #:*s3-directory*
           #:*s3-secret-key*
           #:*s3-access-key*
           #:*s3-browser-secret-key*
           #:*s3-browser-access-key*
           #:*s3-endpoint*
           #:check-s3-active
           #:decode-conspack-with-interning
           #:*potato-standalone*
           #:sorted-list
           #:sorted-list-insert
           #:sorted-list-remove
           #:sorted-list-first-item
           #:plain-sorted-list
           #:cl-containers-sorted-list
           #:receptacle-sorted-list
           #:*state-server-reader-expire-time*
           #:*state-server-reader-high-prio*
           #:*state-server-reader-low-prio*
           #:msgl-stop-queue
           #:msgl-push
           #:msgl
           #:msgl-request-stop-queue
           #:with-msgl
           #:send-rabbitmq-message-to-state-server
           #:*db-user*
           #:*db-password*
           #:s3-enabled-p
           #:with-memcached-warnings-muffled
           #:*slashcommand-request-exchange-name*
           #:*slashcommand-unrouted-command-exchange*
           #:*slashcommand-unrouted-command-queue*
           #:json-bind
           #:*gcm-unread-state-exchange-name*
           #:*session-notifications-exchange-name*
           #:truncate-string
           #:*apns-exchange-name*
           #:*apns-management-exchange-name*
           #:*apns-management-queue-name*
           #:*force-https*
           #:s3-wasabi-enabled))

(defpackage :potato.common.timer
  (:use :cl :potato.common)
  (:export
   #:make-timer-queue
   #:unschedule-timer
   #:update-timer
   #:schedule-timer
   #:timer
   #:stop-timer-queue))

(defpackage :potato.common.memcached
  (:use :cl :potato.common)
  (:export #:*memcached-port*
           #:*memcached-hostname*
           #:init-memcached
           #:*memcached-port*
           #:*memcached-hostname*
           #:find-cached-object-if-exists
           #:with-memcached-check
           #:with-memcached))

(defpackage :potato.common.application
  (:use :cl :potato.common)
  (:export #:define-component
           #:start-component
           #:stop-component))

(in-package :potato.common)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *compile-decl*
    (let ((p (find-package "POTATO-OVERRIDE")))
      (if p
          (symbol-value (find-symbol "*OVERRIDE-DECLS*" p))
          '(optimize (speed 0) (safety 3) (debug 3))))))

(defvar *debug* t)
(defvar *force-https* nil)
