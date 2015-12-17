(asdf:defsystem #:potato-index
  :description "Index manager for Potato"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "Proprietary"
  :serial t
  :depends-on (:potato-common
               :alexandria
               :cl-solr
               :containers
               :cl-markup
               :string-case)
  :components ((:module "src/index"
                        :serial t
                        :components ((:file "package")
                                     (:file "potato-index")))))
