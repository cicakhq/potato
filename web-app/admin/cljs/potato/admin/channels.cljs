(ns potato.admin.channels
  (:require [om.core :as om :include-macros true]
            [om.dom :include-macros true]
            [goog.dom]
            [goog.dom.xml]
            [goog.dom.forms]
            [goog.events]
            [goog.events.EventType]
            [goog.style]
            [goog.dom.classlist]
            [goog.ui.IdleTimer]
            [goog.Uri]
            [goog.Uri.QueryData]
            [goog.History]
            [goog.history.Html5History]
            [cljs-http.client :as http]
            [clojure.string]
            [clojure.set]
            [cljs.core.async  :as async]
            [cljsjs.moment]
            [cljs.pprint])
  (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]]))

(defonce state (atom {:channels []}))

(defonce init-domain-id (aget js/window "domainId"))

(defn- request-channel-list []
  (go (let [{msg :body} (async/<! (http/post "/admin/find-channel-list" {:json-params {:domain init-domain-id}}))]
        (om/transact! (om/root-cursor state) [:channels]
          (fn [_]
            (sort-by :name (:channels msg)))))))

(defn- channel-list-panel [app]
  (apply om.dom/ul nil
         (map (fn [channel]
                (om.dom/li nil
                  (:name channel)))
              (:channels app))))

(defn- channels-admin [app owner]
  (reify
      om/IDisplayName (display-name [_] "channels-admin")
      om/IInitState (init-state [_] {})
      om/IWillMount (will-mount [_] (request-channel-list))
      om/IRenderState
      (render-state [this _]
        (om.dom/div nil
          (om.dom/div nil "This is the channel admin panel")
          (channel-list-panel app)))))

(defn channels-main []
  (let [root-node (goog.dom/getElement "channels-editor")]
    (om/root channels-admin
             state
             {:shared {}
              :target root-node})))
