(ns ^:figwheel-load potato.admin.core
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
            [cljs.pprint]
            [potato.admin.group-edit]
            [potato.admin.channels])
  (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]]))

(defn main []
  (cljs.pprint/cl-format true "Starting admin")
  (potato.admin.group-edit/group-admin-main))
