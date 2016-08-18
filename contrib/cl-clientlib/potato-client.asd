(asdf:defsystem #:potato-client
  :description "Potato client library"
  :license "Apache"
  :serial t
  :depends-on (:drakma
               :st-json
               :alexandria
               :babel)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "potato-client")))))
