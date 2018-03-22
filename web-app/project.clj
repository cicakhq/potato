(defproject potato "0.1.3-SNAPSHOT"
  :description        "A Potato front-end implemented with OM"
  :url                "http://potato.network"
  :license            {:name "Apache"}
  :min-lein-version   "2.8.1"

  :dependencies [[org.clojure/clojure       "1.9.0"]
                 [http-kit                  "2.3.0-beta2"]
                 [org.clojure/clojurescript "1.10.217" :scope "provided"]
                 [org.omcljs/om             "1.0.0-beta1"]
                 [org.clojure/core.async    "0.4.474"]
                 ;; If cljs-http is upgraded to 0.1.39, the code will fail when compiled with optimisations enabled
                 [cljs-http                 "0.1.44" :exclusions [org.clojure/core.async]]
                 [cljsjs/moment             "2.17.1-1"]]

  :plugins      [[lein-cljsbuild            "1.1.7"] ; Leiningen plugin to make ClojureScript development easy
                 ]

  :clean-targets ^{:protect false} ["resources/public/js"]

  :profiles {:dev {:dependencies [[com.cemerick/piggieback "0.2.2"]
                                  [figwheel-sidecar "0.5.4-6"]]
                   :source-paths ["cljs_src"]
                   :plugins [[lein-figwheel "0.5.14" :exclusions [org.clojure/clojure
                                                                  org.clojure/tools.reader
                                                                  ring/ring-core
                                                                  commons-fileupload
                                                                  clj-time]]
                             [cider/cider-nrepl "0.16.0" :exclusions [org.clojure/clojure
                                                                      org.clojure/tools.nrepl]]]}}

  :cljsbuild {:builds
              [{:id "dev"
                :source-paths ["src/cljs" "env/dev/cljs"]

                ;; the port here must match :figwheel :server-port below.
                :figwheel {:websocket-url  "ws://localhost:10555/figwheel-ws"
                           :on-jsload      "potato.main/on-js-reload"}

                :compiler {:main          potato.main
                           :output-to     "resources/public/js/potato.js"
                           :output-dir    "resources/public/js/out/"
                           :asset-path    "/js/out"
                           :optimizations :none
                           :source-map    true
                           :source-map-timestamp true
                           :cache-analysis true}}

               {:id "test"
                :source-paths ["src/cljs" "env/test/cljs"]

                ;; the port here must match :figwheel :server-port below.
                :figwheel {:websocket-url  "ws://localhost:10555/figwheel-ws"
                           :on-jsload      "potato.main/on-js-reload"}

                :compiler {:main          potato.main
                           :output-to     "resources/public/js/test/test.js"
                           :output-dir    "resources/public/js/test/out"
                           :optimizations :none
                           :asset-path    "/js/test/out"
                           :source-map    true
                           :cache-analysis true}}

               {:id "prod"
                :source-paths ["src/cljs" "env/prod/cljs"]
                :compiler {:main          potato.main
                           :output-to     "resources/public/js/potato.js"
                           :output-dir    "resources/public/js/prod/out"
                           :optimizations :advanced
                           :externs       ["externs-input.js"]
                           :elide-asserts false
                           :pseudo-names false
                           :print-input-delimiter false
                           :pretty-print  false}}

               {:id "admin-dev"
                :source-paths ["admin/cljs" "env/admin-dev/cljs"]
                :figwheel {:websocket-url  "ws://localhost:10555/figwheel-ws"
                           :on-jsload      "potato.admin/on-js-reload"}
                :compiler {:main potato.admin
                           :output-to     "resources/public/js/admin.js"
                           :output-dir    "resources/public/js/admin-out"
                           :asset-path    "/js/admin-out"
                           :optimizations :none
                           :source-map    true
                           :source-map-timestamp true
                           :cache-analysis true}}

               {:id "admin-prod"
                :source-paths ["admin/cljs" "env/admin-prod/cljs"]
                :compiler {:main          potato.admin
                           :output-to     "resources/public/js/admin.js"
                           :output-dir    "resources/public/js/admin-prod/out/"
                           :optimizations :advanced
                           :elide-asserts true
                           :pretty-print  false}}]}

  :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]
                 :welcome (println "Welcome to Potato dev server Clojure REPL.")
                 :init-ns potato.dev}
  :figwheel {:server-port      10555
             :http-server-root "public"
             :nrepl-port       7888})
