(ns potato.search
  (:require  [om.core :include-macros true]
             [om.dom  :include-macros true]
             [goog.dom]
             [goog.dom.forms]
             [goog.style]
             [cljs-http.client :as http]
             [cljs.core.async  :as async])
  (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]]))

(def no-results-text    "No results")
(def search-placeholder "Search")

(def search-message-url         "/search_message")

(defn found-message-component [message owner opts]
  (reify
    om.core/IDisplayName (display-name [_] "found-message-component")
    om.core/IRender
    (render [_]
      (om.dom/figure #js {:className "search-result-message"
                          :onClick (fn [_]
                                     (om.core/set-state! (:parent opts) :search-opened false)
                                     (potato.core/request-range-for-message (:channel message) (:id message)))}
        (om.dom/figcaption #js {:className "search-result-header"}
          (om.dom/address #js {:className "chat-author"} (:sender_name message))
          (potato.core/display-time (:created_date message)))
        (om.dom/blockquote #js {:className "search-result-content"
                                :dangerouslySetInnerHTML #js {:__html (:content message)}} nil)))))

(defn search-component [data owner]
  (reify
    om.core/IDisplayName (display-name [_] "search-component")
    om.core/IInitState
    (init-state [_]
      {:search-opened  false
       :found-messages nil
       :search-channel (async/chan (async/dropping-buffer 1))
       :search-string ""
       :star-only false})
    om.core/IWillMount
    (will-mount [_]
      (let [search-channel (om.core/get-state owner :search-channel)]
        (go-loop []
          (let [_ (async/<! search-channel)]
            (let [new-text (om.core/get-state owner :search-string)]
              (when (>= (count new-text) 2)
                (let [star-only (om.core/get-state owner :star-only)
                      cid (:active-channel data)
                      {found :body} (async/<! (http/post search-message-url {:json-params {:channel cid
                                                                                           :text new-text
                                                                                           :star_only star-only}}))]
                  (let [msglist (:messages found)]
                    (om.core/set-state! owner :found-messages (map #(conj % {:channel cid}) (:messages found))))))))
          (recur))))
    om.core/IDidUpdate
    (did-update [_ _ _]
      ;; Update the vertical position of the search results depending on the position of the input search box
      ;; Modify only the "top" value, and adds a 5px margin
      (let [search-section (om.core/get-node owner)
            bounds (goog.style/getBounds (first (goog.dom/getElementsByTagNameAndClass "input" nil search-section)))]
        (goog.style/setStyle (first (goog.dom/getElementsByTagNameAndClass nil "search-results" search-section))
                             "top" (cljs.pprint/cl-format nil "~dpx" (+ (.-top bounds) (.-height bounds) 5)))))
    om.core/IRenderState
    (render-state [_ {:keys [search-opened found-messages]}]
      (om.dom/section #js {:id "search"}
        (om.dom/input #js {:id "search-input" :type "search" :name "search"
                           :placeholder search-placeholder :title "search"
                           :onKeyUp (fn [event]
                                      (om.core/set-state! owner :search-opened true)
                                      (om.core/set-state! owner :search-string (goog.dom.forms/getValue (.-currentTarget event)))
                                      (async/put! (om.core/get-state owner :search-channel) :search-text-changed)
                                      (.preventDefault event))
                           :onFocus #(om.core/set-state! owner :search-opened true)
                           ;; TODO: :search-opened used to be set to false in the statement below.
                           ;; It was changed to true simply to avoid having the dialog box close
                           ;; every time an attempt was made to click on the checkbox. Obviously
                           ;; this is incorrect, but it was done simply to test the new "favourited only"
                           ;; search feature.
                           :onBlur  #(cljs.pprint/cl-format true "blur, owner=~s" owner)})
        (om.dom/div #js {:id "search-results"
                         :className "search-results"
                         :style (potato.core/display search-opened)}
          (om.dom/div nil
            (om.dom/input #js {:id "star_only"
                               :type "checkbox"
                               :name "star_only"
                               :onChange (fn [event]
                                           ;; TODO: We need to force a reload of the search
                                           ;; results here.
                                           (om.core/set-state! owner :star-only
                                                               (not (nil? (goog.dom.forms/getValue (.-currentTarget event)))))
                                           (async/put! (om.core/get-state owner :search-channel) :checkbox-changed))})
            (om.dom/label #js {:htmlFor "star_only"} " Favourited only")
            (om.dom/button #js {:className "search-close"
                                :href "#"
                                :onClick (fn [_] (om.core/set-state! owner :search-opened false))}
                           "\u2715"))
          (if (> (count found-messages) 0)
            (apply om.dom/div #js {:id "search-result-content"}
                   (om.core/build-all found-message-component found-messages {:opts {:parent owner}}))
            ;; ELSE: No results found, display the a message "no results"
            (om.dom/div #js {:id "search-result-content"} no-results-text)))))))
