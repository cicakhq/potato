(set-env!
 :source-paths   #{"src/cljs"}
 :resource-paths #{"resources"}
 :dependencies   '[[adzerk/boot-cljs   "0.0-2814-3"]
                   [adzerk/boot-reload "0.2.6"]
                   [pandeiro/boot-http "0.6.2"]])

(require
 '[adzerk.boot-cljs   :refer [cljs]]
 '[adzerk.boot-reload :refer [reload]]
 '[pandeiro.boot-http :refer [serve]])

(task-options!
 pom  {:project     'potato
       :version     "0.2.0-SNAPSHOT"
       :description "A Potato front-end implemented with OM"
       :license     { "Proprietary" }}
 cljs [:output-to  "public/js/potato.js"
       :source-map true
       :unified    true]
 aot  {:namespace #{'potato.core}}
 jar  {:main 'potato.main})

(deftask dev
  "Start the DEV environment"
  []
  (comp
   (set-env! :source-paths #(conj % "env/dev/cljs"))
   (watch)
   (reload :on-jsload 'app/init)
   (repl   :server true)
   (cljs   :optimizations :none)
   (core/main)))

(deftask dev-repl
  "Connect to the REPL started by the DEV task"
  []
  (repl :client true))

(deftask release []
  (comp
   (set-env! :source-paths #(conj % "env/prod/cljs"))
   (cljs :optimizations :advanced)
   (aot)
   (pom)
   (jar)))

;;; from https://github.com/pandeiro/jamal
;;; see https://github.com/Deraen/saapas/blob/master/build.boot
