(defproject potato "2.0.0-TEST"
  :dependencies [[org.clojure/clojure       "1.8.0"]
                 [org.clojure/clojurescript "1.9.854"]
                 [reagent                   "0.6.0-rc"]
                 [re-frame                  "0.9.4"]
                 [binaryage/devtools        "0.9.4"]
                 [org.clojure/core.async    "0.3.443"]
                 [cljs-http                 "0.1.43" :exclusions [org.clojure/core.async]]
                 [secretary                 "1.2.3"]]

  :plugins      [[lein-cljsbuild            "1.1.7"]
                 [lein-figwheel             "0.5.12"]]

  :hooks        [leiningen.cljsbuild]

  :profiles {:dev {:cljsbuild
                   {:builds {:client {:figwheel   {:websocket-url "ws://localhost:3450/figwheel-ws"
                                                   :on-jsload     "potato.recore/main"}
                                      :compiler   {:main          "potato.recore"
                                                   :asset-path    "/js/out"
                                                   :optimizations :none
                                                   :source-map    true
                                                   :source-map-timestamp true}}}}}
             :prod {:cljsbuild
                    {:builds {:client {:compiler  {:optimizations :advanced
                                                   :elide-asserts true
                                                   :pretty-print  false}}}}}}

  :figwheel {:server-port 3450
             :repl        true}

  :clean-targets ^{:protect false} ["resources/public/js"]

  :cljsbuild {:builds {:client {:source-paths ["src/cljs2"]
                                :compiler     {:output-to     "resources/public/js/potato.js"
                                               :output-dir    "resources/public/js/out"}}}})
