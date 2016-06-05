(asdf:Defsystem #:potato-client-clim
  :description "Potato client in CLIM"
  :license "Apache"
  :serial t
  :depends-on (:mcclim
               :drakma)
  :components ((:module "src"
                        :serial t
                        :components ((:file "main")))))
