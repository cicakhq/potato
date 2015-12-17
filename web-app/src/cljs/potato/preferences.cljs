;; ClojureScript by Mathieu Legrand <mathieu@legrand.im>
(ns potato.preferences
  (:require  [om.core :include-macros true]
             [om.dom  :include-macros true]
             [cljs.core.async  :as async]))

(def preferences-text   "Preferences")
(def done-text          "Done")

(defn open-screen [owner screen]
  (async/put! (:preferences-chan (om.core/get-shared owner)) screen))

(defn notifications [app owner]
  (reify
    om.core/IDisplayName (display-name [_] "preferences/notifications")
    om.core/IRender
    (render [_]
      (om.dom/section
       #js {:className "potato-preferences notifications"}
       (if (not (.hasOwnProperty js/window "Notification"))
         (om.dom/p "Your browser does not support notifications"))
       (if (and (.hasOwnProperty js/window "Notification") (not= (.-permission js/Notification) "granted"))
         (om.dom/div nil
                     (om.dom/p nil (om.dom/b nil "Desktop Notifications are currently disabled"))
                     (om.dom/p nil "We strongly recommend enabling them.")
                     (om.dom/p #js {:className "desktop-enablebutton"}
                               (om.dom/a #js {:className "desktop-enablebutton"
                                              :onClick   (fn [_] (.requestPermission js/Notification #(println "done")))}
                                         "Enable Desktop Notifications"))))
       (if (and (.hasOwnProperty js/window "Notification") (= (.-permission js/Notification) "granted"))
         (om.dom/fieldset nil
                          (om.dom/legend #js {:className "green"}
                                         "Desktop Notifications enabled!"
                                         (om.dom/a #js {:style #js {:cursor "pointer"}
                                                        :className "notification-test"
                                                        :onClick   #(new js/Notification "Potato notification"
                                                                         #js { :body "Hey! it works." })}
                                                   "Send test notification"))))))))


(defn advanced [app owner]
  (reify
    om.core/IDisplayName (display-name [_] "preferences/advanced")
    om.core/IRender
    (render [_]
      (om.dom/section #js {:className "potato-preferences"} "no preference here for now"))))

(defn preferences [app owner]
  (reify
    om.core/IDisplayName (display-name [_] "preferences")
    om.core/IInitState
    (init-state [_]
      {:current-menu "notifications"})
    om.core/IRenderState
    (render-state [_ {:keys [current-menu]}]
      (om.dom/div
       #js {:id "potato-preferences"
            :className "potato-preferences animated fadeInDown"
            :onClick (fn [e] ;; prevent Propagation for now
                       (.stopPropagation e)
                       (.preventDefault e))}
       (om.dom/header #js {:className "potato-preferences"}
                      (om.dom/div #js {:className "hgroup"} (om.dom/h1 nil preferences-text))
                      (om.dom/div #js {:style #js {:cursor "pointer"}
                                       :onClick (fn [e] (open-screen owner false)
                                                  (.preventDefault e))}
                                  (om.dom/a #js {:className "done"} done-text)))
       (om.dom/main   #js {:className "potato-preferences"}
                      (om.dom/nav #js {:className "potato-preferences"}
                                  (om.dom/a #js {:className (str "menu" (if (= current-menu "notifications") " active"))
                                                 :onClick #(om.core/set-state! owner :current-menu "notifications")} "Notifications")
                                  (om.dom/a #js {:className (str "menu" (if (= current-menu "advanced") " active"))
                                                 :onClick #(om.core/set-state! owner :current-menu "advanced")} "Advanced"))
                      (cond
                        (= current-menu "notifications")
                        (om.core/build notifications app)
                        (= current-menu "advanced")
                        (om.core/build advanced app)
                        :else
                        (om.dom/section #js {:className "potato-preferences"} nil)))))))
