(defpackage :potato.sso
  (:use :cl :potato.common)
  (:documentation "SSO support for Potato")
  (:export #:sso-init
           #:ldap-check-user))
