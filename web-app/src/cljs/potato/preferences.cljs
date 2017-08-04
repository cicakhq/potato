;; ClojureScript by Mathieu Legrand <mathieu@legrand.im>
(ns potato.preferences
  (:require  [om.core :include-macros true]
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
      (p/createElement :section {:class-name "potato-preferences notifications"}
       (if (not (.hasOwnProperty js/window "Notification"))
         (p/createElement :p nil "Your browser does not support notifications"))
       (if (and (.hasOwnProperty js/window "Notification") (not= (.-permission js/Notification) "granted"))
         (p/createElement :div nil
           (p/createElement :p nil (p/createElement :b nil "Desktop Notifications are currently disabled"))
           (p/createElement :p nil "We strongly recommend enabling them.")
           (p/createElement :p {:className "desktop-enablebutton"}
                     (p/createElement :a {:class-name "desktop-enablebutton"
                                          :on-click   (fn [_] (.requestPermission js/Notification #(println "done")))}
                                      "Enable Desktop Notifications"))))
       (if (and (.hasOwnProperty js/window "Notification") (= (.-permission js/Notification) "granted"))
         (p/createElement :fieldset nil
                          (p/createElement :legend {:class-name "green"}
                                         "Desktop Notifications enabled!"
                                         (p/createElement :a {:style #js {:cursor "pointer"}
                                                              :class-name "notification-test"
                                                              :on-click   #(new js/Notification "Potato notification"
                                                                               { :body "Hey! it works." })}
                                                          "Send test notification"))))))))


(defn advanced [app owner]
  (reify
    om.core/IDisplayName (display-name [_] "preferences/advanced")
    om.core/IRender
    (render [_]
      (p/createElement :section {:class-name "potato-preferences"} "no preference here for now"))))

(defn preferences [app owner]
  (reify
    om.core/IDisplayName (display-name [_] "preferences")
    om.core/IInitState
    (init-state [_]
      {:current-menu "notifications"})
    om.core/IRenderState
    (render-state [_ {:keys [current-menu]}]
      (p/createElement :div {:id "potato-preferences"
                             :class-name "potato-preferences animated fadeInDown"
                             :on-click (fn [e] ;; prevent Propagation for now
                                         (.stopPropagation e)
                                         (.preventDefault e))}
       (p/createElement :header {:class-name "potato-preferences"}
                        (p/createElement :div {:class-name "hgroup"} (p/createElement :h1 nil preferences-text))
                        (p/createElement :div {:style #js {:cursor "pointer"}
                                               :on-click (fn [e] (open-screen owner false)
                                                           (.preventDefault e))}
                                  (p/createElement :a {:class-name "done"} done-text)))
       (p/createElement :main {:class-name "potato-preferences"}
                      (p/createElement :nav {:class-name "potato-preferences"}
                                  (p/createElement :a {:class-name (str "menu" (if (= current-menu "notifications") " active"))
                                                       :on-click #(om.core/set-state! owner :current-menu "notifications")} "Notifications")
                                  (p/createElement :a {:class-name (str "menu" (if (= current-menu "advanced") " active"))
                                                       :on-click #(om.core/set-state! owner :current-menu "advanced")} "Advanced"))
                      (cond
                        (= current-menu "notifications")
                        (om.core/build notifications app)
                        (= current-menu "advanced")
                        (om.core/build advanced app)
                        :else
                        (p/createElement :section {:class-name "potato-preferences"} nil)))))))
