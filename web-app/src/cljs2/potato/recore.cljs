(ns potato.recore
  (:require [goog.events :as events]
            [reagent.core :as reagent]
            [devtools.core :as devtools]))

(devtools/set-pref! :bypass-availability-checks true)
(devtools/set-pref! :dont-detect-custom-formatters true)
(devtools/install!)
(enable-console-print!)

(defn main-panel
  []
  [:div "Hello"])

(defn ^:export main
  []
  (reagent/render [main-panel]
                  (.getElementById js/document "potato-root")))
