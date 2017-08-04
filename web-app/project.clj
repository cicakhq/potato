(defproject potato    "0.2.0-SNAPSHOT"
  :description        "A Potato web-frontend"
  :url                "http://potato.network"
  :license            {:name "Apache"}
  :min-lein-version   "2.7.1"

  :dependencies [[org.clojure/clojure       "1.8.0"   ]
                 [org.clojure/clojurescript "1.9.854" :scope "provided"]
                 [org.clojure/core.async    "0.3.443" ]
                 [cljsjs/preact             "7.1.0-0" ]
                 [http-kit                  "2.1.19"  ]
                 [cljs-http                 "0.1.43" :exclusions [org.clojure/core.async]]
                 [cljsjs/moment             "2.9.0-0" ]]

  :plugins      [[lein-cljsbuild            "1.1.7"]
                 [lein-figwheel             "0.5.12"]]

  :clean-targets ^{:protect false} ["resources/public/js"]

  :profiles {:dev  {:cljsbuild
                    {:builds {:client {:figwheel  {:websocket_url "ws://localhost:3450/figwheel-ws"
                                                   :on-jsload     "potato.core/main"}
                                       :compiler  {:main          "potato.core"
                                                   :asset_path    "/js/out"
                                                   :optimizations :none
                                                   :source_map    true
                                                   :source_map_timestamps true}}}}}
             :prod {:cljsbuild
                    {:builds {:client {:compiler  {:optimizations :advanced
                                                   :elide_assers  true
                                                   :pretty_print  false}}}}}}

  :cljsbuild {:builds {:client {:source_paths ["src/cljs"]
                                :compiler     {:output_to   "resources/public/js/potato.js"
                                               :output_dir  "resources/public/js/out"}}}}

  :figwheel {:server-port  3450})
