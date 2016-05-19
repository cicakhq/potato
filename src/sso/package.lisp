(defpackage :potato.sso
  (:use :cl :potato.common)
  (:documentation "SSO support for Potato")
  (:export #:sso-init
           #:ldap-check-user))

(defpackage :potato.regblock
  (:use :cl :potato.common)
  (:documentation "Specify constraints as to who is allowed to register")
  (:export #:regblock-init))
