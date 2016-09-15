(defpackage :potato-client
  (:use :cl)
  (:export #:*connection*
           #:list-domains
           #:connection
           #:connection/api-key
           #:connection/url-prefix
           #:send-message
           #:load-channel-tree
           #:load-channel
           #:request-error/code
           #:request-error/reason
           #:request-error
           #:potato-client-error
           #:list-users
           #:listener-loop
           #:subscribe-to-channel
           #:message-history
           #:load-domain
           #:user-image))
