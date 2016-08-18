(defpackage :potato-client
  (:use :cl)
  (:export
   #:*connection*
   #:list-domains
   #:connection
   #:connection/api-key
   #:connection/url-prefix))
