(ns potato.main
  (:require [potato.core]
            [potato.state]
            [figwheel.client :as figwheel :include-macros true]
            [cljs.core.async :refer [put!]]))

(enable-console-print!)

(defn on-js-reload []
  (println "Reloading")
  ;; touch the app-state to force rerendering
  ;; the alternative is calling core/main, which re-establish the EventSource connection
  (swap! potato.state/global update-in [:__figwheel_counter] inc))

(println "loaded")
(potato.core/main)
