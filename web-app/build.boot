(set-env!
  ; Test path can be included here as source-files are not included in JAR
  ; Just be careful to not AOT them
  :source-paths #{"src/cljs" "src/less" "env/prod/cljs"}
  :resource-paths #{"resources"}
  :dependencies '[
                  [adzerk/boot-cljs       "1.7.228-1"  :scope "test"]
                  [deraen/boot-less       "0.5.0"      ]
                  [cljs-http              "0.1.30"     ]
                  [org.omcljs/om             "0.9.0"]
                  [cljsjs/moment             "2.9.0-0"]

                  ;; For boot-less
                  [org.slf4j/slf4j-nop    "1.7.21"     :scope "test"]])

(require
  '[adzerk.boot-cljs      :refer [cljs]]
  '[deraen.boot-less      :refer [less]])


;(deftask package
;  "Build the package"
;  []
;  (comp
;    (less :compression true)
;    (cljs :optimizations :advanced)
;;    (aot)
;;    (pom)
;;    (uber)
;;    (jar)
;    (target)
;    ))
(deftask dev 
  "Start the dev environment... "
  []
  (comp
   (watch)
   (less)
   (cljs-repl) ; order is important!!
   (cljs  :optimizations :none)))
