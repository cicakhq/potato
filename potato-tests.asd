(asdf:defsystem #:potato-tests
  :description "Test cases for potato"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "Proprietary"
  :serial t
  :depends-on (:potato
               :fiveam)
  :components ((:module "src/tests"
                        :serial t
                        :components ((:file "package")
                                     (:file "potato-tests")
                                     (:file "notifications-tests")
                                     (:file "db-tests")
                                     (:file "user-tests")
                                     (:file "domain-tests")
                                     (:file "workflow-tests")
                                     (:file "channel-tests")
                                     (:file "private-tests")
                                     (:file "state-server-tests")
                                     (:file "timer-tests")
                                     (:file "common-string-tests")
                                     (:file "msgl-tests")))))
