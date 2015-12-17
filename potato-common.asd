(asdf:defsystem #:potato-common
  :description "Common function shared by the different modules"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "Apache"
  :serial t
  :depends-on (:alexandria
               :bordeaux-threads
               :clouchdb
               :log4cl
               :trivial-backtrace
               :string-case
               :uiop
               :zs3
               :secure-random
               :pooler
               :lparallel
               :fset
               :cl-containers
               :cl-solr
               :cl-rabbit
               :cl-rabbit-async)
  :components ((:module "src/common"
                        :serial t
                        :components ((:file "package")
                                     (:file "application")
                                     (:file "log")
                                     (:file "common")
                                     (:file "sorted-list")
                                     (:file "fset-map")
                                     (:file "s3-util")
                                     (:file "common-db")
                                     (:file "file")
                                     (:file "rabbitmq")
                                     (:file "memcached")
                                     (:file "timer")
                                     (:file "format-message")
                                     (:file "msgl-queue")))))
