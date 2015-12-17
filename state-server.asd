(asdf:defsystem #:state-server
  :description "Server keeping the state of users in the channels"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "Proprietary"
  :serial t
  :depends-on (:bordeaux-threads
               :alexandria
               :trivial-timers
               :iolib
               :string-case
               :cl-ppcre
               :queues
               :queues.simple-queue
               :fset
               :potato-common)
  :components ((:module "src/state-server"
                        :components ((:file "package")
                                     (:file "id-gen")
                                     (:file "state-server")))))
