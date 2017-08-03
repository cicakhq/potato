(defn mount-root []
  (reagent/render [views/main-panel]
          (.getElementById js/document "potato-root")))

(defn ^:export main []
  (mount-root))
