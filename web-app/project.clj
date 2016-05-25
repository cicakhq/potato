(defproject
  potato              "0.1.3-SNAPSHOT"
  :description        "A Potato front-end implemented with OM"
  :url                "http://potato.network"
  :license            {:name "Apache"}
  :min-lein-version   "2.5.3"

  :dependencies [[org.clojure/clojure       "1.7.0"]
                 [http-kit "2.1.8"]
                 [org.clojure/clojurescript "1.7.170" :scope "provided"]
                 [org.omcljs/om             "0.9.0"]
                 [org.clojure/core.async    "0.2.374"]
                 ;; If cljs-http is upgraded to 0.1.39, the code will fail when compiled with optimisations enabled
                 [cljs-http                 "0.1.30" :exclusions [org.clojure/core.async]]
                 [cljsjs/moment             "2.9.0-0"]]

  :plugins      [[lein-cljsbuild            "1.1.2"] ; Leiningen plugin to make ClojureScript development easy
                 ]

  :clean-targets ^{:protect false} ["resources/public/js"]

  :profiles {:dev {:dependencies [[com.cemerick/piggieback "0.2.1"]
                                  [figwheel-sidecar "0.5.0-1"]]
                   :source-paths ["cljs_src"]
                   :plugins [[lein-figwheel "0.5.0-1" :exclusions [org.clojure/clojure
                                                                   org.clojure/tools.reader
                                                                   ring/ring-core
                                                                   commons-fileupload
                                                                   clj-time]]
                             [cider/cider-nrepl "0.13.0-SNAPSHOT" :exclusions [org.clojure/clojure
                                                                               org.clojure/tools.nrepl]]]}}

  :cljsbuild {:builds
              [{:id "dev"
                :source-paths ["src/cljs" "env/dev/cljs"]

                ;; the port here must match :figwheel :server-port below.
                :figwheel {:websocket-url  "ws://localhost:10555/figwheel-ws"
                           :on-jsload      "potato.main/on-js-reload"}

                :compiler {:main          potato.main
                           :output-to     "resources/public/js/potato.js"
                           :output-dir    "resources/public/js/out"
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
                           :optimizations :advanced
                           :elide-asserts true
                           :pretty-print  false}}]}

  :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]
                 :welcome (println "Welcome to Potato dev server Clojure REPL.")
                 :init-ns potato.dev}                 
  :figwheel {:server-port      10555
             :http-server-root "public"
             :nrepl-port       7888})
