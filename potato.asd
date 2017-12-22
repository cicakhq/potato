(asdf:defsystem #:potato
    :description "Group chat application"
    :license "Apache"
    :serial t
    :depends-on (:lofn
                 :clouchdb
                 :html5-notification
                 :st-json
                 :alexandria
                 :local-time
                 :trivial-utf-8
                 :ironclad
                 :secure-random
                 :closer-mop
                 :string-case
                 :cl-memcached
                 :iolib
                 :trivial-gray-streams
                 :receptacle
                 :secure-random
                 :cl-who
                 :log4cl
                 :cl-smtp
                 :cl-conspack
                 :cl-base64
                 :hunchensocket
                 ;;:wookie
                 ;;:websocket-driver
                 :north
                 :cl-markup
                 :mailgun
                 :potato-common
                 :state-server
                 :potato-index)
    :components ((:module "src/potato"
                          :serial t
                          :components ((:file "package")
                                       (:file "misc")
                                       (:file "usocket")
                                       (:file "misc-core")
                                       (:file "build-id")
                                       (:file "metaclasses")
                                       (:file "db")
                                       (:file "image-convert")
                                       (:file "content-processor")
                                       (:file "user")
                                       (:file "email")
                                       (:file "session")
                                       (:file "login")
                                       (:file "oauth-north")
                                       (:file "user-image")
                                       (:file "settings")
                                       (:file "domain")
                                       (:file "group")
                                       (:file "channel")
                                       (:file "domain-nickname")
                                       (:file "channel-nickname")
                                       (:file "user-display-config")
                                       (:file "notification-keywords")
                                       (:file "user-state")
                                       (:file "message")
                                       (:file "user-notification")
                                       (:file "search")
                                       (:file "typing")
                                       (:file "notifications")
                                       (:file "session-notification")
                                       (:file "rabbitmq-notifications")
                                       (:file "rabbitmq-channels")
                                       (:file "private")
                                       (:file "upload")
                                       (:file "upload-plain")
                                       (:file "message-update")
                                       (:file "workflow")
                                       (:file "chat-server")
                                       (:file "ws-server")
                                       (:file "api")
                                       (:file "api-android")
                                       (:file "register")
                                       (:file "register2")
                                       (:file "gcm")
                                       (:file "slashcommand")
                                       (:file "cmd-giphy")
                                       (:file "web")
                                       (:file "private-web")
                                       (:file "channel-web")
                                       (:file "domain-web")
                                       (:file "domain-welcome-web")
                                       (:file "userlist-web")
                                       (:file "user-notification-web")
                                       (:file "admin")
                                       (:file "views")
                                       (:file "serialiser")
                                       (:file "potato-main")
                                       (:file "ping-sender")
                                       (:file "youtube-processor")
                                       (:file "github-processor")
                                       (:file "wikipedia-processor")
                                       (:file "xkcd-processor")
                                       (:file "commands")
                                       (:file "db-fixes")))))
