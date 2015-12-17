(ns potato.admin
  (:require [potato.admin.core]
            [figwheel.client :as figwheel :include-macros true]
            [cljs.core.async :refer [put!]]))

(enable-console-print!)

(defn on-js-reload []
  (println "Reloading")
  (swap! potato.admin.group-edit/state update-in [:__figwheel_counter] inc))

(println "loaded")
(potato.admin.core/main)
