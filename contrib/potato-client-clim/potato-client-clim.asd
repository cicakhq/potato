(asdf:defsystem #:potato-client-clim
  :description "Potato client in CLIM"
  :license "Apache"
  :serial t
  :depends-on (:mcclim
               :drakma
               :log4cl
               :st-json
               :flexi-streams
               :containers)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "misc")
                                     (:file "main")))))
