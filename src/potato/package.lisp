(defpackage :potato
  (:use :cl :potato.common)
  (:documentation "Corporate chat server")
  (:export #:*listen-port*
           #:*listen-address*
           #:*external-listen-address*
           #:*external-listen-port*
           #:*smtp-server-host*
           #:*smtp-server-port*
           #:*smtp-username*
           #:*smtp-password*
           #:*smtp-ssl*
           #:start-server
           #:start-server-debug
           #:reset-db
           #:*build-id*
           #:start-message-processor-server
           #:start-ping-sender-thread
           #:start-email-updates-server
           #:start-server-all-services
           #:setup-initial-database
           #:*websocket-listen-port*
           #:*external-websocket-listen-address*
           #:*allow-passwordless-login*
           #:*allow-password-recovery*
           #:*user-registration-validation-function*
           #:init-dev-db
           #:*allowed-origin*
           #:*mailgun-key*
           #:*email-type*
           #:*mailgun-user-domain*
           #:*allow-registration-without-domain*))

(defpackage :potato.core
  (:use :cl :potato :potato.common)
  (:export #:start-main
           ;; Core functions that should be in a different package
           #:generic-init
           #:ensure-user-id
           #:define-handler-fn-login
           #:define-json-handler-fn-login
           #:cached-image-data
           #:with-authenticated-user
           #:user/id
           #:current-user
           #:load-group-with-check
           #:load-channel-with-check
           #:channel/id
           #:make-message-from-couchdb
           #:load-message-range-with-check
           #:message->json
           #:message-cd->json
           #:group/id
           #:load-user
           #:user/image-name
           #:user
           #:format-timestamp
           #:parse-timestamp
           #:check-user-in-channel
           #:make-message
           #:save-message
           #:channel/name
           #:user/description
           #:user/primary-email
           #:user/email-addresses
           #:raise-permission-error
           #:permission-error
           #:domain
           #:group
           #:domain/id
           #:channel
           #:find-groups-in-domain
           #:find-channels-for-group
           #:domain-user-role
           #:domain-type
           #:find-default-group-in-domain
           #:ensure-domain
           #:ensure-user
           #:load-domains-for-user
           #:domain/name
           #:load-user-by-email
           #:user/password
           #:setup-user-session
           #:user/update-password
           #:email->user-id
           #:user/activated-p
           #:user/api-token
           #:potato-error/message
           #:user/default-image-name
           #:load-domain-with-check
           #:user-is-in-domain-p
           #:domain-email-invitation
           #:create-channel
           #:ensure-domain-id
           #:user-is-in-channel-p
           #:check-user-in-domain
           #:raise-web-parameter-error
           #:web-parameter-error
           #:potato-error/response-status
           #:channel/group
           #:group/name
           #:group/domain
           #:group/users
           #:group/type
           #:group/email-domains
           #:load-message-log
           #:save-message-modification
           #:load-message
           #:message/channel
           #:message/text
           #:message/from-name
           #:message/from
           #:message/created-date
           #:message/id
           #:message 
           #:message/file-list
           #:user-is-admin-in-group-p
           #:message-detailed->json
           #:channel/domain
           #:channel-users
           #:channel-users/users
           #:channel-users/group-type
           #:channel-users/domain
           #:channel-users/group
           #:channel-users/name
           #:channel-users/channel-id
           #:find-descriptions-for-users
           #:user-session/user
           #:find-current-user-session
           #:user-role-for-group
           #:ensure-channel-id
           #:ensure-channel
           #:find-user-source
           #:user-source/user-notification-source
           #:user-source/unread-source
           #:user-source/user-id
           #:signout-user
           #:refresh-user
           #:find-channel-source
           #:channel-source/state-source
           #:notification-message-cd->json-html
           #:poll-for-updates-and-return-from-request
           #:user-email
           #:user-email/user
           #:user-email/email
           #:users-in-channel
           #:add-user-to-channel
           #:remove-user-from-channel
           #:notification-message-cd->json-alist
           #:notification-message-cd->json-text
           #:*authenticator-function*
           #:session-value
           #:*external-user-check-function*
           #:couchdb-id
           #:notification-keywords-for-users-in-channel
           #:load-channel-users-with-check
           #:run-future
           #:channel-source/typing-source
           #:send-typing-end-notification-to-state-server
           #:user-descriptions-for-channel-members
           #:domain/domain-type
           #:message/updated
           #:message/active-image
           #:update-channel-visibility-for-user
           #:notification-keywords/ping-delay
           #:notification-keywords/keywords
           #:user/android-gcm-key
           #:message-updated/from
           #:message-updated/date
           #:message-updated/deleted
           #:*search-result-surrounding-rows*
           #:message/extra-html
           #:clear-unread-state-for-user-in-channel
           #:load-notification-keywords-for-user
           #:notification-keywords/min-email-delay
           #:user-unread-state-rabbitmq-message/user
           #:user-unread-state-rabbitmq-message/channel
           #:user-unread-state-rabbitmq-message/count
           #:user-unread-state-rabbitmq-message
           #:save-user
           #:find-channels-for-user
           #:channel/topic
           #:domain/email-domains
           #:*max-message-size*
           #:is-allowed-email-p
           #:domain/public
           #:domain/join-default
           #:user/new-login
           #:make-and-save-domain
           #:make-potato-url
           #:find-domain-email-invitations-for-domain
           #:load-message-with-check
           #:update-message-star-with-check
           #:message/deleted
           #:message/star-users
           #:message/image
           #:message/updated-date
           #:find-domain-nickname
           #:domain-nickname/domain
           #:domain-nickname/nickname
           #:domain-nickname
           #:channel-nickname
           #:find-domain-nickname-from-domain-id
           #:channel-nickname/channel
           #:channel-nickname/nickname
           #:channel-nickname/domain
           #:find-channel-nickname-from-channel-id
           #:find-channel-nickname
           #:find-channel-id-for-nicknames
           #:set-nickname-for-domain
           #:set-nickname-for-channel
           #:raise-not-found-error
           #:create-group
           #:+GROUP-USER-TYPE-ADMIN+
           #:define-constant
           #:+GROUP-USER-TYPE-PRIVATE+
           #:+GROUP-USER-TYPE-USER+
           #:add-user-to-group
           #:domain-user
           #:remove-user-from-group
           #:load-domains-for-user%obsolete
           #:update-role-for-group-user
           #:disable-channel
           #:potato-error
           #:ensure-message
           #:domain-user/user
           #:domain-user/domain
           #:domain-user/role
           #:load-message-range
           #:trim-string
           #:user-nickname
           #:user/nickname
           #:nickname-is-in-use-p
           #:valid-user-nickname-p
           #:display-config
           #:display-config/user
           #:display-config/timezone
           #:load-display-config-for-user
           #:format-timestamp-for-display-config
           #:check-group-access
           #:send-typing-start-notification-to-state-server
           #:channel/deleted
           #:load-channel-users
           #:name-for-private-channel-counterpart
           #:load-user-email-by-email
           #:message/hidden-users
           #:update-message-hidden-with-check
           #:update-domain-user-role
           #:clear-user-password
           #:common-user-domains
           #:validate-cookie-and-find-user
           #:load-available-domains-for-emails
           #:load-available-domains-for-user
           #:remove-user-from-domain
           #:update-user-role-in-domain))

(defpackage :potato.views
  (:use :cl :clouchdb :parenscript)
  (:export #:init-views
           #:init-views-if-needed))

(defpackage :potato.image-convert
  (:use :cl :potato :potato.common :potato.core)
  (:export #:convert-user-image
           #:start-chat-image-processor-thread
           #:*imagemagick-convert-program*))

(defpackage :potato.user-image
  (:use :cl :potato :potato.common :potato.core)
  (:export #:user-save-image
           #:user-load-image
           #:persisted-entry/couchdb-revision
           #:image-url-for-user
           #:user-image-name
           #:download-user-image))

(defpackage :potato.db
  (:use :cl :potato :potato.common :potato.core)
  (:export #:init-db
           #:persist-error
           #:load-instance
           #:save-instance
           #:remove-instance
           #:load-instance-from-doc
           #:load-attachment
           #:save-attachment
           #:persisted-entry/couchdb-id
           #:persisted-entry/couchdb-revision
           #:persisted-entry-class
           #:persisted-entry
           #:call-clouchdb-update-function
           #:persisted-entry-class/persisted-allow-missing-value
           #:couchdb-missing-value
           #:invoke-view-and-load-instances
           #:define-hook-fn
           #:flush-cache-for-object-if-enabled 
           #:make-random-couchdb-id
           #:couchdb-id
           #:couchdb-revision
           #:couchdb-attachments
           #:couchdb-type-for-class
           #:persisted-rt
           #:remove-instance-nofail
           #:flush-cache-for-instance
           #:persisted-entry-is-value-updated
           #:persisted-entry-clear-modifications-list
           #:persisted-entry/loaded-p))

(defpackage :potato.search
  (:use :cl :potato :potato.common :potato.core)
  (:export #:search-messages-json))

(defpackage :potato.upload
  (:use :cl :potato :potato.common)
  (:export #:file
           #:file/name
           #:file/created-date
           #:file/key
           #:file/confirmed-p
           #:file/channel
           #:make-download-location
           #:download-file-by-filesource
           #:upload-file-by-name
           #:file/mime-type
           #:file/message
           #:file/id
           #:file/location
           #:*file-upload-directory*
           #:*default-upload-location*
           #:download-file-to-client
           #:process-multipart-upload))

(defpackage :potato.user-notification
  (:use :cl :potato :potato.common)
  (:export #:process-user-notifications-for-new-message
           #:find-user-notification-source
           #:user-notification-source
           #:mark-notifications-for-user-channel
           #:mark-notifications-by-id
           #:user-notification
           #:user-notification/read
           #:user-notification/sender-description
           #:user-notification/text
           #:user-notification-state
           #:check-and-update-last-notification-date
           #:user-notification/channel
           #:process-user-notification-message-and-build-json
           #:message/deleted
           #:user-notification/message-id
           #:user-notification/created-date))

(defpackage :potato.workflow
  (:use :cl :potato :potato.common :potato.core)
  (:export #:add-user-to-domain-with-check
           #:register-user
           #:domain-user-role
           #:ensure-domain
           #:add-email-invitation-for-domain
           #:remove-email-invitation-for-domain
           #:send-message-to-channel
           #:create-channel-with-check
           #:add-user-to-domain))

(defpackage :potato.register
  (:use :cl :potato :potato.common :potato.core)
  (:export #:register2-post-handler))

(defpackage :potato.email
  (:use :cl :potato :potato.common :potato.core)
  (:export #:mail-descriptor
           #:send-email
           #:start-email-sender-thread
           #:*potato-sender-address*
           #:make-mail-for-user))

(defpackage :potato.settings
  (:use :cl :potato :potato.common))

(defpackage :potato.private
  (:use :cl :potato :potato.common)
  (:export #:find-private-channel-for-users
           #:*privgrp-group-prefix*
           #:find-private-channels-for-domain-and-user
           #:find-chat-counterpart))

(defpackage :potato.web
  (:use :cl :potato :potato.common)
  (:export #:show-template-stream-with-default-parameters
           #:make-group-channel-tree-for-user-and-domain
           #:*allow-create-domain*))

(defpackage :potato.api
  (:use :cl :potato :potato.common))

(defpackage :potato.message-update
  (:use :cl :potato :potato.common)
  (:export
   #:start-message-update-thread))

(defpackage :potato.content-processor
  (:use :cl :potato :potato.common)
  (:export #:start-message-content-processor-thread
           #:register-url-processor))

(defpackage :potato.ping-sender
  (:use :cl :potato :potato.common)
  (:export #:start-ping-sender-thread))

(defpackage :potato.rabbitmq-notifications
  (:use :cl :potato :potato.common)
  (:export #:call-with-consumer
           #:process-long-poll
           #:+all-services+
           #:create-and-bind-notifications-queue-async
           #:process-message
           #:declare-notifications-queue-async
           #:add-new-channel-binding
           #:add-new-channel-binding-async
           #:verify-queue-name
           #:*session-notification-unknown-slashcommand*
           #:*session-notification-option*
           #:remove-channel-binding
           #:remove-channel-binding-async))

(defpackage :potato.rabbitmq-channels
  (:use :cl :potato :potato.common)
  (:export #:process-channel-update))

(defpackage :potato.usocket
  (:use :cl)
  (:export #:monitor-connection))

(defpackage :potato.commands
  (:use :cl :potato :potato.common)
  (:export #:run-command))

(defpackage :potato.ws-server
  (:use :cl :potato :potato.common)
  (:export #:start-ws-server))

(defpackage :potato.slashcommand
  (:use :cl :potato :potato.common)
  (:export #:command-processor-loop
           #:process-incoming-slashcommand))

(defpackage :potato.gcm
  (:use :cl :potato :potato.common)
  (:export #:start-gcm-listener
           #:*gcm-authorisation-key*
           #:register-gcm
           #:gcm-enabled-p
           #:gcm-registration
           #:update-unread-subscription
           #:parse-provider-name
           #:*gcm-sender*))

(defpackage :potato.oauth-north
  (:use :cl :potato :potato.common))
