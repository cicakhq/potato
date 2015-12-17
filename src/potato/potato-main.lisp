(in-package :potato)

(declaim #.potato.common::*compile-decl*)

(defun start-server ()
  (potato.common.application:start-component 'main))

(potato.common.application:define-component message-processor-server
  (:dependencies potato.common::generic potato.upload::upload)
  (:start
   (potato.common.application:start-component 'potato.common::generic)
   (potato.image-convert:start-chat-image-processor-thread)
   (potato.content-processor:start-message-content-processor-thread)
   (potato.email:start-email-sender-thread)
   (log:info "Message processor started")))

(potato.common.application:define-component email-updates-server
  (:dependencies potato.common::generic potato.db::db)
  (:start
   (potato.ping-sender:start-ping-sender-thread)
   (log:info "Email updates started")))

(potato.common.application:define-component main
  (:dependencies potato.core::main-web-server potato.ws-server::ws-server potato.slashcommand::slashcommand-default))

(defun start-server-debug ()
  (log4cl:set-log-level log4cl:*root-logger* :debug)
  (potato.common.application:start-component 'all-services)
  (values))

(potato.common.application:define-component all-services
  (:dependencies state-server::state-server potato-index::index-manager main message-processor-server email-updates-server))

(defun reset-db ()
  (clouchdb:delete-db :if-missing :ignore)
  (with-user-notification-db
    (clouchdb:delete-db :if-missing :ignore))
  (potato.common.application:start-component 'generic)
  (cl-solr:delete-data "*:*")
  (setup-initial-database))

(defun setup-initial-database ()
  (potato.common.application:start-component 'potato.common.memcached::memcached)
  (potato.common.application:start-component 'potato.db::db)
  (potato.views:init-views)
  (cl-memcached:mc-flush-all)
  (values))
