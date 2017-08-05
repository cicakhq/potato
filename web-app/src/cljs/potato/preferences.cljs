;; ClojureScript by Mathieu Legrand <mathieu@legrand.im>
(ns potato.preferences
  (:require
   [ cljsjs.preact    :as p     ]
   [ cljs.core.async  :as async ]
   [ potato.component :as c     ]))

(def preferences-text   "Preferences")
(def done-text          "Done")

(defn open-screen [owner screen]
  (async/put! (:preferences-chan (om.core/get-shared owner)) screen))

(c/defcomponent Notifications
  :name "preferences/notifications"
  [_]
  (p/h :section {:class-name "potato-preferences notifications"}
       (if (not (.hasOwnProperty js/window "Notification"))
         (p/h :p nil "Your browser does not support notifications"))
       (if (and (.hasOwnProperty js/window "Notification") (not= (.-permission js/Notification) "granted"))
         (p/h :div nil
              (p/h :p nil (p/h :b nil "Desktop Notifications are currently disabled"))
              (p/h :p nil "We strongly recommend enabling them.")
              (p/h :p {:className "desktop-enablebutton"}
                   (p/h :a {:class-name "desktop-enablebutton"
                            :on-click   (fn [_] (.requestPermission js/Notification #(println "done")))}
                        "Enable Desktop Notifications"))))
       (if (and (.hasOwnProperty js/window "Notification") (= (.-permission js/Notification) "granted"))
         (p/h :fieldset nil
              (p/h :legend {:class-name "green"}
                   "Desktop Notifications enabled!"
                   (p/h :a {:style #js {:cursor "pointer"}
                            :class-name "notification-test"
                            :on-click   #(new js/Notification "Potato notification"
                                              { :body "Hey! it works." })}
                        "Send test notification"))))))


(c/defcomponent Advanced
  :name "preferences/advanced"
  [_]
  (p/h :section {:class-name "potato-preferences"} "no preference here for now"))

(c/defcomponent Preferences
  :name "preferences"
  ;;; +FIXME: initial-state {:current-menu "notifications"}
  [{:keys [current-menu]}] ;;; +FIXME missing owner cursor?
  (p/h :div {:id "potato-preferences"
             :class-name "potato-preferences animated fadeInDown"
             :on-click (fn [e] ;; prevent Propagation for now
                         (.stopPropagation e)
                         (.preventDefault e))}
       (p/h :header {:class-name "potato-preferences"}
            (p/h :div {:class-name "hgroup"} (p/h :h1 nil preferences-text))
            (p/h :div {:style #js {:cursor "pointer"}
                       :on-click (fn [e] (open-screen owner false)
                                   (.preventDefault e))}
                 (p/h :a {:class-name "done"} done-text)))
       (p/h :main {:class-name "potato-preferences"}
            (p/h :nav {:class-name "potato-preferences"}
                 (p/h :a {:class-name (str "menu" (if (= current-menu "notifications") " active"))
                          :on-click #(om.core/set-state! owner :current-menu "notifications")} "Notifications")
                 (p/h :a {:class-name (str "menu" (if (= current-menu "advanced") " active"))
                          :on-click #(om.core/set-state! owner :current-menu "advanced")} "Advanced"))
            (cond
              (= current-menu "notifications")
              (Notifications)
              (= current-menu "advanced")
              (Advanced)
              :else
              (p/h :section {:class-name "potato-preferences"} nil)))))
