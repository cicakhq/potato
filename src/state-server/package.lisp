(defpackage :state-server
  (:use :cl :potato.common)
  (:documentation "Server tracking users in the channels")
  (:export #:start-state-server
           #:decode-name
           #:encode-name
           #:add-user
           #:remove-user))
