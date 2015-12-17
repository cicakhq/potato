(asdf:defsystem #:potato-sso
  :description "SSO support for Potato"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "Proprietary"
  :serial t
  :depends-on (:cl-base64
               :trivial-ldap
               :potato
               :cl-gss)
  :components ((:module "src/sso"
                        :serial t
                        :components ((:file "package")
                                     (:file "sso")))))
