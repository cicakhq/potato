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
               :flexichain
               :receptacle
               :potato-client)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "misc")
                                     (:file "flexichain-output-history")
                                     (:file "largelist")
                                     (:file "user")
                                     (:file "messages")
                                     (:file "notifications")
                                     (:file "main")))))
