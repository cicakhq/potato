(asdf:defsystem #:potato-client-clim
  :description "Potato client in CLIM"
  :license "Apache"
  :serial t
  :depends-on (:mcclim
               :drakma
               :log4cl
               :st-json
               :flexi-streams
               :bordeaux-threads
               :lparallel
               :string-case
               :containers
               :potato-client)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "misc")
                                     (:file "messages")
                                     (:file "notifications")
                                     (:file "main")))))
