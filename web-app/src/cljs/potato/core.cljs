;;; ClojureScript by Mathieu Legrand <mathieu@legrand.im>
;;; To connect:
;;;   M-x cider-connect  localhost  7888
;;;   (figwheel-sidecar.repl-api/cljs-repl)
;;;
;;; To use the latest Clojurescript:
;;;   M-x cider-jack-in
;;;   (use 'figwheel-sidecar.repl-api)
;;;   (start-figwheel!)
;;;   (cljs-repl)

(ns ^:figwheel-load potato.core
  (:use [cljs-hash.sha1 :only [sha1]])
  (:require [om.core :as om :include-macros true]
            [om.dom :include-macros true]
            [goog.dom]
            [goog.dom.xml]
            [goog.events]
            [goog.events.EventType]
            [goog.style]
            [goog.dom.classlist]
            [goog.ui.IdleTimer]
            [goog.Uri]
            [goog.Uri.QueryData]
            [goog.History]
            [goog.history.Html5History]
            [goog.crypt.Sha1]
            [cljs-http.client :as http]
            [clojure.string]
            [clojure.set]
            [cljs.core.async  :as async]
            [cljsjs.moment]
            [cljs.pprint]
            [cljs-hash.goog]
            [potato.state]
            [potato.urls]
            [potato.eventsource2]
            [potato.keyboard]
            [potato.emoji]
            [potato.preferences]
            [potato.search]
            [potato.fineuploader]
            [potato.mathjax])
  (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]]))

(defonce current-build-id (aget js/window "currentBuildId"))

;; Localization related
(def is-or-are-typing   "~1{~#[none~;~a~;~a and ~a~:;~@{~#[~;and ~]~a~^, ~}~]~:} ~:*~1{~#[are~;is~:;are~]~:} typing")
(def nonbreak-space     "\u00a0")
(def message-deleted    "message deleted")
(def message-updated    "updated")
(def placeholder        "Hello from ClojureScript")
(def channels-text      "Channels")
(def private-text       "Conversations")
(def users-text         "Users")
(def megabytes-text     "MB")
(def kilobytes-text     "kB")
(def bytes-text         "B")
(def loading-history    "Loading history…")
(def unread-text        "(unread)")
(def active-text        "")
(def uploading-file-text "Uploading file…")
(def needs-your-permission-text "Potato needs your permission to")
(def enable-notifications-text  "enable desktop notifications")

(def max-image-width    360)
(def max-image-height   360)

(def draft-marker       "~~") ;; internal

(defn logret [msg v]
  (println "Log " msg ": " v)
  v)

(defn position [f l]
  ((fn [x pos]
     (cond (empty? x) nil
           (f (first x)) pos
           :else (recur (rest x) (inc pos))))
   l 0))

(defn pos-and-index [f l]
  ((fn [x pos]
     (if (empty? x)
       nil
       (let [v (first x)]
         (if (f v)
           [pos v]
           (recur (rest x) (inc pos))))))
   l 0))

(defn update-values-map [m f & args]
  "For each entry in hash map m, update its value by replacing its
  content by the value returned by function f applied on the original
  value."
  (reduce (fn [r [k v]] (assoc r k (apply f v args))) {} m))

(defn- state-root []
  (om/root-cursor potato.state/global))

(defn make-channel-descriptor [id name topic private hide unread-count]
  {:id id
   :name name
   :topic topic
   :private private
   :messages (sorted-map)
   :range nil
   :typing #{}
   :unread-count unread-count
   :has-autocomplete-menu nil
   :hide hide
   :users {}})

(defn- scrollToBottom [e] (.scrollIntoView e false))

(defn path-from-components [& args]
  (clojure.string/join "/"  args))

(defn starts-with? [s prefix]
  "Returns true if string 's' starts with 'prefix'."
  (let [prefix-len (count prefix)]
    (and (>= (count s) prefix-len)
         (= (subs s 0 prefix-len) prefix))))

(defn- send-update-star [message-id enable-p]
  (http/post potato.urls/update-star {:json-params {:message message-id
                                                    :enable enable-p}}))

(defn- update-index-of-message [msg]
  (let [i (:update-index msg)]
    (or i 0)))

(defn- increment-message-index [msg]
  (conj msg {:update-index (+ (update-index-of-message msg) 1)}))

(defn %update-messages-in-channel-for-users [ch uids]
  (update-in ch [:messages]
    (fn [messages]
      ;; The following function performs a reduction on all
      ;; the messages in the channel. However, the function
      ;; will only conj the updated value if it needs
      ;; changing. If not, the original list will be left
      ;; alone. This works since the full list of messages is
      ;; passed in as both the initial list as well as the
      ;; list of elements to reduce.
      (reduce (fn [l [k v]]
                (if (contains? uids (:from v))
                  (conj l {k (increment-message-index v)})
                  l))
              messages messages))))

(defn refresh-messages-for-users [app uids]
  "Update the message index for all messages in all channels whose
author is a user in uids. The uids parameter must be a set of user
id's. Returns the updated value."
  (update-in app [:channels]
    (fn [channels]
      (update-values-map channels (fn [ch]
                                    (%update-messages-in-channel-for-users ch uids))))))

(defn hide-channel-button-clicked [cid]
  (http/post potato.urls/update-channel {:json-params {:channel cid :show false}})
  (om/transact! (state-root) [:channels cid]
    (fn [channel]
      (when (nil? channel)
        (throw (str "Attempt to hide nonexistent channel " cid)))
      (conj channel {:hide true}))))

(defn close-channel-button-clicked [cid]
  (http/post potato.urls/remove-channel {:json-params {:channel cid}})
  (om/transact! (state-root) [:channels]
    (fn [channels]
      (dissoc channels cid))))

(defn update-channel-state [app channel-info]
  ;;(cljs.pprint/cl-format true "Updating channel state: ~s" channel-info)
  (let [private (= (:group_type channel-info) "PRIVATE")]
    (update-in app [:channels (:id channel-info)]
      (fn [channel]
        (if channel
          ;; When updating the channel info, the private flag
          ;; may have changed. This happens when a members
          ;; update has been received before the channel
          ;; information was received. In this case the user
          ;; list is added to a stub channel data structure,
          ;; but when that structure is created that code
          ;; does not know if the channel is private or not.
          ;; Thus, it needs to be correctly updated here.
          (conj channel {:name (:name channel-info)
                         :topic (:topic channel-info)
                         :private private
                         :hide (:hide channel-info)
                         :unread-count (:unread_count channel-info)})
          ;; ELSE: Add a new channel to the map
          (make-channel-descriptor (:id channel-info)
                                   (:name channel-info)
                                   (:topic channel-info)
                                   private
                                   (:hide channel-info)
                                   (:unread_count channel-info)))))))

(defn update-channel-list-for-channel-info [channel-info]
  (om/transact! (state-root)
      (fn [app]
        (update-channel-state app channel-info))))

(defn request-channel-info-for-all-channels [ready-callback]
  "Send a request to the server to retrieve a list of all channels the
  user is a member of, and update the application state."
  (go (let [current-domain       (:current-domain (deref (state-root)))
            {channel-info :body} (async/<! (http/post potato.urls/load-channels-for-user
                                                      {:json-params {:domain (:id current-domain)}}))]
        (om/transact! (state-root)
            (fn [app]
              ((fn [a chlist]
                 (if (empty? chlist)
                   a
                   (recur (update-channel-state a (first chlist))
                          (rest chlist))))
               app (:channels channel-info))))
        (ready-callback))))

(defn request-channel-info [cid]
  "Given channel id 'cid', request details about the channel and
  update the application state with this information."
  (go (let [{channel-info :body} (async/<! (http/post potato.urls/load-channel {:json-params {:channel cid}}))]
        (update-channel-list-for-channel-info (:channel channel-info)))))

(defn handle-unread-notification [e]
  (let [cid (:channel e)
        count (:count e)]
    (cljs.pprint/cl-format true "Unread notification, data: ~s" e)
    (om/transact! (state-root) [:channels]
      (fn [channels]
        (if (get channels cid)
          (update-in channels [cid]
            (fn [v] (conj v {:unread-count count :hide (if (= count 0) (:hide v) false)})))
          channels)))))

(defn update-user-to-name-map-with-id-description-list [app user-list]
  "Given a list of users, update the user-to-name-map in app with the
  new data and return the updated app state."
  ;; Each element in 'user-list' contains a map of the following form:
  ;;   {:id "id" :description "name" :image_name "image_name"}
  ;; :image_name can be missing, in which case the previous value
  ;; should be kept. This happens when a join/leave message is
  ;; received.
  (let [updated (update-in app [:user-to-name-map]
                  (fn [m]
                    (let [user-list-as-map (reduce (fn [r v]
                                                     (conj r {(:id v) {:description (:description v)
                                                                       :image-name (:image_name v)}}))
                                                   {} user-list)
                          updated-list (reduce (fn [r [k v]]
                                                 (let [updated-user (get user-list-as-map k)]
                                                   (conj r {k {:description (or (:description updated-user)
                                                                                (:description v))
                                                               :image-name (or (:image-name updated-user)
                                                                               (:image-name v))}})))
                                               {} m)]
                      (clojure.set/union updated-list (reduce dissoc user-list-as-map (keys updated-list))))))]
    (refresh-messages-for-users updated (into #{} (map :id user-list)))))

(defn handle-user-name-change [e]
  (cljs.pprint/cl-format true "Got user name change: ~s" e)
  (let [uid (:user e)]
    (om/transact! (state-root)
        (fn [app]
          (let [updated (update-in app [:user-to-name-map uid]
                          (fn [v]
                            {:description (:description e)
                             :image-name (:image_name e)}))]
            (refresh-messages-for-users updated #{uid}))))))

(defn request-details-for-users [app user-list]
  (cljs.pprint/cl-format true "Requesting details for users: ~s" user-list)
  (go
    (let [{user-list-result :body} (async/<! (http/post "/user_details"
                                                        {:json-params {:domain (-> app :current-domain :id)
                                                                       :ids user-list}}))]
      (om/transact! (state-root)
          (fn [app]
            (update-user-to-name-map-with-id-description-list app (:users user-list-result)))))))

(defn update-app-for-user-notifications [app cid uid-list add-p sync-p]
  ;; Update the user state
  (let [res (if (get (:channels app) cid)
              ;; Update the user list in the existing channel structure
              (update-in app [:channels cid :users]
                (fn [users]
                  (reduce conj (if sync-p
                                 (update-values-map (reduce dissoc users uid-list)
                                                    #(conj % {:active false}))
                                 users)
                          (map (fn [uid] {uid {:active add-p}}) uid-list))))
              ;; ELSE: We don't have this channel in the channel list, add a stub instance.
              ;;       This will happen when the channel data has not been received yet.
              (update-in app [:channels cid]
                (fn [_]
                  (conj (make-channel-descriptor cid "" "" false true 0)
                        {:users (reduce conj {} (map (fn [uid]
                                                       {uid {:active add-p}})
                                                     uid-list))}))))]
    ;; Request details on users which are not included in the user to name map
    (let [user-to-name-map (:user-to-name-map res)
          needed-uids (filter #(not (get user-to-name-map %)) uid-list)]
      (when (not (empty? needed-uids))
        (request-details-for-users res needed-uids)))
    ;; Return the updated structure
    res))

(defn handle-channel-user-notification [e]
  (om/transact! (state-root)
      (fn [app]
        (if (= (:add-type e) "sync")
          (update-app-for-user-notifications app (:channel e) (map :id (:users e)) true true)
          (let [add-p (case (:add-type e)
                        "add" true
                        "remove" false
                        (throw "Unexpected user notification type"))]
            (update-app-for-user-notifications app (:channel e) [(:user e)] add-p false))))))

(defn update-user-list [cid members]
  (om/transact! (state-root)
      (fn [app]
        (let [res (update-user-to-name-map-with-id-description-list app members)]
          (update-in res [:channels cid :users]
            (fn [users]
              (let [new-user-list (remove #(get users (:id %)) members)]
                (reduce conj users (map (fn [user] {(:id user) {:active false}}) new-user-list)))))))))

(defn request-user-list-for-channel [cid]
  (go (let [{user-list-result :body} (async/<! (http/post potato.urls/members {:json-params {:channel cid}}))]
        (update-user-list cid (:members user-list-result)))))

(defn clean-notification-text [t]
  (.log js/console (goog.dom/htmlToDocumentFragment t)))

(defn mark-notifications []
  (let [ids (map :id (:outstanding-notifications (deref potato.state/global)))]
    (when (not (empty? ids))
      (om/transact! (state-root) [:outstanding-notifications]
        (fn [notifications]
          (let [m (into #{} ids)]
            (remove #(m (:id %)) notifications))))
      (http/post "/clear_notification" {:json-params {:ids ids}}))))

(defn fixup-input-text [s]
  "Takes the raw input string from the text field and cleans it up
  prior to sending to the server. This includes trimming excessive
  whitespace and adding newlines around the triple-backquote."
  (if (nil? s)
    ""
    (-> s
        clojure.string/trim
        (clojure.string/replace #"([^\n])```" "$1\n```")
        (clojure.string/replace #"```([^\n])" "```\n$1"))))

(defn show-notification [tag message body image]
  (let [notification (new js/Notification message #js {:body body :icon image :tag tag})]
    (js/setTimeout (fn [] (.close notification)) 5000)))

(defn handle-user-notification [e]
  "Handler that is called when a notification of type 'usernot'
  arrives. This message should lead to a popup being displayed in the
  user interface."
  (let [id (:id e)          ; Unique ID for this specific notification
        user (:user e)      ; ID of the user that sent the message
        user-name (:user_description e) ; Name of the user that sent the message
        channel (:channel e) ; ID of the channel the message was sent on
        text (:text e)      ; The text content of the message
        created-date (:created_date e) ; The timestamp when the message was created
        type (:notification_type e)] ; Notification type, one of "PRIVATE", "MENTION" or "WORD".
    ;; Push the notification to the list of outstandning notifications
    (om/transact! (state-root) [:outstanding-notifications]
      (fn [notifications]
        (conj notifications e)))
    ;; Display a notification popup
    (if (.hasOwnProperty js/window "Notification")
      (let [image (path-from-components potato.urls/user-image user "a")]
        (case type
          ;; Message sent on a private channel
          "PRIVATE" (show-notification id (cljs.pprint/cl-format nil "Private message from ~a" user-name) text image)
          ;; Mention of the user's name
          "MENTION" (show-notification id (cljs.pprint/cl-format nil "~a mentioned you" user-name) text image)
          ;; A keyword was mentioned on a channel
          "WORD"    (show-notification id (cljs.pprint/cl-format nil "Update from ~a" user-name) text image)
          ;; Unexpected notification type, just display a generic message
          (show-notification id "Notification from potato" text image))))))

(defn handle-type-notification [e async-channel]
  (let [cid (:channel e)
        uid (:user e)
        begin-p (cond (= (:add-type e) "begin") true
                      (= (:add-type e) "end")   false
                      :else                 (throw "Unexpected type for type notification"))]
    (when (get (:channels @potato.state/global) cid)
      (om/transact! (state-root) [:channels cid :typing]
        (fn [v]
          (if begin-p
            (conj v uid)
            (disj v uid))))
      (cljs.pprint/cl-format true "Updating typing state, cid=~s, uid=~s, type=~s, state=~s"
                             cid uid (:add-type e) (:typing (get (:channels @potato.state/global) cid)))
      ;; We need to tell the input field that the list of users have changed
      (async/put! async-channel [:type (-> @potato.state/global :channels (get cid) :typing)]))))

(defn- handle-channel-change [e]
  (om/transact! (state-root) [:channels (:channel e)]
    (fn [channel]
      (conj channel {:name (:name e) :topic (:topic e)}))))

(defn- update-message [channel msgid fn]
  (let [updated-channel (if (get (:messages channel) msgid)
                          (update-in channel [:messages msgid] fn)
                          channel)]
    (let [range (:range channel)]
      (if (and range (get (:range-messages range) msgid))
        (update-in updated-channel [:range :range-messages msgid] fn)
        updated-channel))))

(defn- handle-update-star [e]
  (let [cid (:channel e)
        msgid (:message e)
        enable-p (:add e)]
    (om/transact! (state-root) [:channels cid]
      (fn [channel]
        (update-message channel msgid
                        (fn [msg]
                          (increment-message-index (conj msg {:star_users (if enable-p
                                                                            [(:id (:current-user @potato.state/global))]
                                                                            [])}))))))))

(defn dispatch-notification-entry [entry async-channel]
  (cljs.pprint/cl-format true "Got server message: ~s" entry)
  (case (:type entry)
    ;; Dispatch message events to the async channel
    "m"
    (async/put! async-channel [:m (:c entry)])
    ;;
    ;; Typing notifications
    "type"
    (handle-type-notification entry async-channel)
    ;;
    ;; Handle unread messages
    "unread"
    (handle-unread-notification entry)
    ;; Handle channel user updates
    "cu"
    (handle-channel-user-notification entry)
    ;;
    ;; User notifications
    "usernot"
    (handle-user-notification entry)
    ;; User name change
    "user-name-change"
    (handle-user-name-change entry)
    ;; Channel metadata change
    "channel-change"
    (handle-channel-change entry)
    ;; Star state updated
    "update-star"
    (handle-update-star entry)
    ;; Log any unhandled event types
    (cljs.pprint/cl-format true "Unknown type: ~s" entry)))

(defn make-polling-connection [channel-id]
  (let [async-channel (async/chan)]
    (potato.eventsource2/set-callback-fn! (fn [event]
                                            (dispatch-notification-entry event async-channel)))
    (potato.eventsource2/set-error-fn! (fn [error-p]
                                         (cljs.pprint/cl-format true "Setting error: ~s" error-p)
                                         (om/transact! (om/root-cursor potato.state/connection) [:error]
                                           (constantly error-p))))
    (potato.eventsource2/start-notification-handler channel-id)
    async-channel))

(defn fetch-messages [start current-channel-id]
  (let [c (async/chan)]
    (go (let [{messages :body} (async/<! (http/post "/history" {:json-params {:channel current-channel-id
                                                                              :num_messages 50
                                                                              :start start}}))]
          (async/>! c (:messages messages))))
    c))

(defn display [show]
  (if show
    #js {:display "block"}
    #js {:display "none"}))

(defn myself-view [user owner]
  (reify
    om/IDisplayName (display-name [_] "myself-view")
    om/IRender
    (render [_]
      (om.dom/footer #js {:id "myself"}
        (om.dom/section #js {:className "myself-wrapper"
                             :onClick   (fn [e] (potato.preferences/open-screen owner true) (.preventDefault e))}
          (om.dom/h1 nil
              (om.dom/a #js {:className "myself-name" :href potato.urls/settings} (:name user)))
          (om.dom/a #js {:className "myself-menu" :href potato.urls/settings}))))))

(defn channel-in-list [[this-channel-id this-channel-details] owner {:keys [current-channel-id] :as opts}]
  (reify
    om/IDisplayName (display-name [_] "channel-in-list")
    om/IInitState
    (init-state [_]
      {:show-close false})
    om/IRenderState
    (render-state [_ {:keys [show-close]}]
      (om.dom/li #js {:data-id this-channel-id
                      :className (if (= this-channel-id current-channel-id) "current")
                      :onMouseEnter #(om/set-state! owner :show-close true)
                      :onMouseLeave #(om/set-state! owner :show-close false)}
        (if (= this-channel-id current-channel-id)
          (str (:name this-channel-details))
          (om.dom/a #js {:className "channel"
                         :href      (str potato.urls/channel-root "/" this-channel-id)} ;; DDC
            (om.dom/span nil
                (:name this-channel-details)
              (if (> (:unread-count this-channel-details) 0)
                (if (:private this-channel-details)
                  (om.dom/span #js {:className "private-unread"} "\u00a0(unread)\u00a0\ud83d\udd08" )
                  (om.dom/span #js {:className "channel-unread"} "\u00a0(unread)"))))))
        (om.dom/a #js {:className "close"
                       :style (display (and show-close (not= this-channel-id current-channel-id)))
                       :onClick (if (:private this-channel-details)
                                  #(hide-channel-button-clicked this-channel-id)
                                  #(close-channel-button-clicked this-channel-id))}
          nil)))))

(defn- lower-case-channel-name [[key value]]
  (clojure.string/lower-case (:name value)))

(defn- filter-channels [filter-fn channels-list]
  (filter #(and (filter-fn (second %))
                (not (:hide (second %)))) channels-list))

(defn channels-list [data owner]
  (reify
    om/IDisplayName (display-name [_] "channels-list")
    om/IRender
    (render [_]
      (om.dom/nav #js {:id "left"}
        (om.dom/section #js {:id "domain"}
          (om.dom/h1 #js {:onClick (fn [e] (aset js/window "location" potato.urls/domain))}
            (:name (:current-domain data))))
        (om.dom/section #js {:id "channels"}
          (om.dom/div #js {:id "channels-list"}
            (om.dom/h1 nil channels-text)
            (apply om.dom/ul nil
                   (om/build-all channel-in-list
                                 (sort-by lower-case-channel-name
                                          (filter-channels #(not (:private %)) (:channels data)))
                                 {:opts {:current-channel-id (:active-channel data)}})))
          (let [private-channels (sort-by lower-case-channel-name
                                          (filter-channels #(:private %) (:channels data)))]
            (when (> (count private-channels) 0)
              (om.dom/div #js {:id "conversations-list"}
                (om.dom/h1 nil private-text)
                (apply om.dom/ul nil
                       (om/build-all channel-in-list private-channels
                                     {:opts {:current-channel-id (:active-channel data)}}))))))
        (om/build myself-view (:current-user data))))))

(defn start-private-chat [uid]
  (go (let [current-domain (:current-domain (deref (state-root)))
            {dest :body}   (async/<! (http/post potato.urls/start-private-chat {:json-params {:user uid :domain (:id current-domain)}}))]
        (when (:channel dest)
          (aset js/window "location" (str potato.urls/channel-root "/" (:channel dest)))))))

(defn user-in-list [[uid name active] owner]
  (reify
    om/IDisplayName (display-name [_] "user-in-list")
    om/IRender
    (render [_]
      (om.dom/li #js {:className (if active "online-highlight")
                      :onClick   (if uid #(start-private-chat uid))} name
                      (if active (str nonbreak-space active-text))))))

(defn channel-header [channel owner]
  (reify
    om/IDisplayName (display-name [_] "channel-header")
    om/IRender
    (render [_]
      (om.dom/header #js {:id "channel"}
        (om.dom/div  #js {:className "hgroup"}
          (om.dom/h1 nil (:name channel))   ;; channel-name
          (om.dom/h2 nil (:topic channel))) ;; channel-topic
        (om.dom/aside nil
            (om.dom/a {:href "#"} ""))))))

(defn display-time [time]
  (om.dom/time #js {:dateTime time} (.fromNow (js/moment time))))

;;; ISeqable for NodeList from https://groups.google.com/forum/#!topic/clojure/unHrE3amqNs
(extend-type js/NodeList
  ISeqable
  (-seq [array] (array-seq array 0)))

(defn printable-size [size]
  (let [kb 1024
        mb (* kb 1024)]
    (if (> size mb)
      (str (Math/floor (/ size mb)) nonbreak-space megabytes-text)
      (if (> size kb)
        (str (Math/floor (/ size kb)) nonbreak-space kilobytes-text)
        (str size nonbreak-space bytes-text)))))

(defn message-attachment [attachment owner]
  (reify
    om/IDisplayName (display-name [_] "message-attachment")
    om/IRender
    (render [_]
      (om.dom/span #js {:className "chat-attachment"}
        (om.dom/a #js {:href (:location attachment)} (:name attachment))
        (om.dom/span #js {:className "chat-file-size"} (printable-size (:size attachment)))))))

;; (potato.core/request-range-for-message "7e77259b7b5bfde0bb2964217f083c0e" "msg-7e77259b7b5bfde0bb2964217f083c0e-2014-12-27T14:09:53.178690Z_00000")
(defn request-range-for-message [cid msgid]
  (go (let [{range :body} (async/<! (http/post "/range" {:json-params {:message msgid :rows 20}}))
            msglist (:messages range)]
        (om/transact! (state-root) [:channels cid]
          (fn [channel]
            (assoc channel :range {:message-id msgid
                                   :range-messages (reduce (fn [l msg]
                                                             (conj l {(:id msg) msg}))
                                                           (sorted-map) msglist)}))))))

(defn close-message-history-range [cid]
  (om/transact! (state-root) [:channels cid :range] (fn [_] nil)))

(defn edit-message [message opts]
  (go (let [{msg :body} (async/<! (http/post potato.urls/load-chat {:json-params {:message (:id message) :type "text"}}))]
         ((:on-edit opts) msg))))

(defn delete-message [message]
  (http/post potato.urls/delete-chat {:json-params {:message (:id message)}}))

(defn gear-menu [message owner opts]
  (reify
    om/IDisplayName (display-name [_] "gear-menu")
    om/IDidMount
    (did-mount [_]
      (let [menu-el       (om/get-node owner)
            blockquote-el (goog.dom/getAncestorByClass menu-el "chat-blockquote")
            offset        (goog.style/getPageOffset blockquote-el)
            outerWidth    (.-width (goog.style/getSize blockquote-el))
            left          (- (+ outerWidth (.-x offset)) (.-width (goog.style/getSize menu-el)))]
        (goog.style/setPosition menu-el left (.-y offset))))
    om/IRender
    (render [_]
      (apply om.dom/menu #js {:id "chat-popup-menu" :type "popup"}
             (map (fn [menuentry]
                    (om.dom/menuitem #js {:label        (:label menuentry)
                                          :onClick      (:onclick menuentry)
                                          :onMouseEnter #(goog.dom.classlist/add    (.-currentTarget %) "chat-popup-item-active")
                                          :onMouseLeave #(goog.dom.classlist/remove (.-currentTarget %) "chat-popup-item-active")}
                      (:label menuentry)))
                  [ {:label "Edit"   :onclick #(edit-message message opts)}
                    {:label "Delete" :onclick #(delete-message message)}])))))

(defn compute-image-size [current-width current-height max-width max-height]
  (let [scaling-width (if (> current-width max-width)
                        (/ current-width max-width)
                        1)
        scaling-height (if (> current-height max-height)
                         (/ current-height max-height)
                         1)
        scaling        (/ 1 (max scaling-width scaling-height))]
    [(Math/floor (* current-width scaling)) (Math/floor (* current-height scaling))]))

(defn- create-redirect-link [a-link]
  (goog.Uri/create (.-protocol js/location)   ; Protocol
                   nil                        ; User info
                   (.-hostname js/location)   ; Hostname
                   (.-port     js/location)   ; Port number
                   "/redirect"                ; Path
                   (goog.Uri.QueryData/createFromMap #js {:location (.getAttribute a-link "href")})
                   nil nil))                  ; Fragment and IgnoreCase

(defn message-quote [message owner opts]
  (reify
    om/IDisplayName (display-name [_] "message-quote")
    om/IInitState
    (init-state [_]
      {:menu-opened false})
    om/IDidMount
    (did-mount [_]
      (doseq [u (goog.dom/getElementsByTagNameAndClass "em" "user" (om/get-node owner))]
        (if (== (.getAttribute u "user-id") (:id (:current-user (deref potato.state/global))))
          (goog.dom.classlist/add u "me")))
      (let [chat-content-text-node (goog.dom/getElementByClass "chat-content-text" (om/get-node owner))]
        (doseq [a-link (goog.dom/getElementsByTagNameAndClass "a" nil chat-content-text-node)]
          (goog.dom.xml/setAttributes a-link #js {:target  "_blank"
                                                  :onClick (str "href=\"" (create-redirect-link a-link) "\"")}))))
    om/IRenderState
    (render-state [_ {:keys [menu-opened editing-callback editing editable]}]
      (let [current-user (:current-user (deref potato.state/global))
            isDeleted    (:deleted message)
            hasGearMenu  (and editable
                              (not isDeleted)
                              (or  (= (:from message) (:id current-user))
                                   (:is-admin? current-user)))
            isEmpty      (== (count (:text message)) 0)]
        (om.dom/blockquote
            (clj->js ((fn [c]
                        (if hasGearMenu
                          (assoc c
                                 :onMouseEnter #(goog.dom.classlist/add (.-currentTarget %) "hover")
                                 :onMouseLeave #(goog.dom.classlist/remove (.-currentTarget %) "hover"))))
                      {:className "chat-blockquote"
                       :style (display (not editing))}))
            (om.dom/div #js {:className "chat-content-text"}
              (if (and (:image message) (not isDeleted))
                (let [[width height] (compute-image-size (:width (:image message))
                                                         (:height (:image message))
                                                         max-image-width
                                                         max-image-height)]
                  (om.dom/img #js {:src (:file (:image message))
                                   :width width :height height
                                   :onClick #(goog.dom.classlist/toggle (.-currentTarget %) "zoom")})))
              (if (and (not isDeleted) (:files message))
                (apply om.dom/div #js {:className (str "chat-attachments" (if isEmpty " empty"))}
                       (om/build-all message-attachment (:files message))))
              (if isDeleted
                (om.dom/span #js {:className "chat-deleted"}
                  message-deleted nonbreak-space (display-time (:updated_date message)))
                (om.dom/div #js {:className (if isEmpty "empty")}
                  (if (and (:unconfirmed message) (:raw_field message))
                    (om.dom/span #js {:dangerouslySetInnerHTML #js {:__html (:raw_field message)}} nil)
                    (om.dom/span #js {:dangerouslySetInnerHTML #js {:__html (:text message)}} nil))
                  (if (:updated message)
                    (om.dom/span #js {:className "chat-updated"}
                      message-updated nonbreak-space (display-time (:updated_date message))))
                  (if (:extra_html message)
                    (om.dom/div #js {:className "extra-html"
                                     :dangerouslySetInnerHTML #js {:__html (:extra_html message)}})))))
          ;; Display the star next to the message
          (when (not (:unconfirmed message))
            (let [message-starred-p (some #{(:id (:current-user @potato.state/global))} (:star_users message))]
              (om.dom/a #js {:className (str "starred " (if message-starred-p "starred-enable" "starred-disable"))
                             :onClick #(send-update-star (:id message) (not message-starred-p))})))
          ;; Display the gear menu
          (when (and hasGearMenu (not (:unconfirmed message)))
            (om.dom/a #js {:className "chat-actions"
                           :onClick   #(om/set-state! owner :menu-opened true)}))
          (when menu-opened
            (goog.events/listen (:root-node (om/get-shared owner))
                                goog.events.EventType/CLICK
                                #(om/set-state! owner :menu-opened false))
            (om/build gear-menu message {:opts {:on-edit editing-callback}})))))))

(defn- at-magic-callback [owner text event editable type]
  (let [cid     (:active-channel (deref potato.state/global))
        channel (get-in (deref potato.state/global) [:channels cid])]
    (when-not (:has-autocomplete-menu channel)
      (om/transact! (state-root)
          [:channels cid :has-autocomplete-menu]
        #(potato.keyboard/get-menu-position editable event)))
    (if (> (count text) 1)
      (async/put! (om/get-shared owner :search-item) {:text (subs text 1) :type type}))
    (when (potato.keyboard/is-UP? event)
      (.preventDefault event)
      (async/put! (om/get-shared owner :move-selected-menuitem) 1))
    (when (potato.keyboard/is-DOWN? event)
      (.preventDefault event)
      (async/put! (om/get-shared owner :move-selected-menuitem) -1))
    (when (or (potato.keyboard/is-TAB? event) (potato.keyboard/is-ENTER? event))
      (.preventDefault event)
      (if (:has-autocomplete-menu channel)
        (when-let [selected-entry (:selected-menu-entry channel)]
          (om/transact! (state-root)
              [:channels cid :has-autocomplete-menu] (fn [] nil))
          (potato.keyboard/create-uneditable-container selected-entry))))))

(defn- at-magic-close! []
  (let [cid (:active-channel (deref potato.state/global))]
    (om.core/transact! (state-root)
                       [:channels cid  :has-autocomplete-menu] (fn [] nil))))

(defn message-view [message owner opts]
  (let [handle-update-or-mount (fn [owner]
                                 (let [node (om/get-node owner)]
                                   (when (and (:use_math message)
                                              (not (om/get-state owner :editing)))
                                     (potato.mathjax/add-node node))))]
    (reify
        om/IDisplayName (display-name [_] "message-view")
        om/IInitState
        (init-state [_]
          {:editing false})
        om/IDidMount
        (did-mount [_]
          (let [user-id     (:id (:current-user (deref potato.state/global)))
                the-node    (om/get-node owner)]
            (handle-update-or-mount owner)
            ;; Check if the screen should be scrolled to this message.
            ;; TODO: We should really only run this code when the
            ;; message is the last one in the list. Right now it's being
            ;; run for all messages, which is never necessary.
            (when (= (update-index-of-message message) 0)
              (if (or (:initial-load message) (:client-top message) (= (:from message) user-id))
                ;; if the message has been inserted by the initial history load, by a scrolling top, or is from me: scroll
                (scrollToBottom the-node)
                ;; else this is a new message from somebody else at the bottom
                (let [rect (.getBoundingClientRect the-node)]
                  (if (and (>= (.-top rect) 0) (>= (.-left rect) 0)
                           (<= (.-bottom rect) (or (.-innerHeight js/window) (.-clientHeight (.documentElement js/document))))
                           (<= (.-right rect) (or (.-innerWidth js/window) (.-clientWidth (.documentElement js/document)))))
                    ;; if the node is visible: scroll
                    (scrollToBottom the-node)
                    ;; if the node isn't visible, test how far we are in the scroll
                    (let [the-ul      (goog.dom/getAncestorByTagNameAndClass the-node "ul" nil)
                          the-article (goog.dom/getAncestorByTagNameAndClass the-node "article" nil)]
                      (if (< (- (.-scrollHeight the-article) (.-scrollTop the-article)) (* 1.5 (.-height (goog.style/getSize the-ul))))
                        ;; more than half a page away, scroll
                        (scrollToBottom the-node)
                        (print "No scroll, too far from bottom")))))))))
        om/IDidUpdate
        (did-update [_ _ _]
          (handle-update-or-mount owner)
          ;; TODO: add scroll control behaviour
          ;; - is the node visible; if it is not but above: realign
          ;; - is the scroll position lower than the existing node
          )
        om/IWillUpdate
        (will-update [_ next-props {:keys [editing]}]
          (when editing
            (let [fieldset            (goog.dom/getElementByClass "chat-entry" (om/get-node owner))
                  keyboard-controller (om/get-shared owner :keyboard-control)
                  typing-chan         (om/get-shared owner :typing-chan)
                  editable            (potato.keyboard/append-editable-div {:parent-node     fieldset
                                                                            :keyboard        keyboard-controller
                                                                            :typing-callback #(async/put! typing-chan true)
                                                                            :id              "edit-entry"
                                                                            :text-content    (:text editing)})
                  cid                   (:active-channel (deref potato.state/global))]
              (potato.keyboard/set-event! editable potato.keyboard/ENTER
                                          {:callback (fn [] (when-not (get-in (deref potato.state/global)
                                                                              [:channels cid :has-autocomplete-menu])
                                                              (let [new-text (fixup-input-text (potato.keyboard/content editable))]
                                                                (http/post potato.urls/update-chat {:json-params {:message (:id message)
                                                                                                                  :text new-text}})
                                                                (potato.keyboard/destroy! editable)
                                                                (potato.keyboard/reset-cursor! keyboard-controller)
                                                                (om/set-state! owner :editing false)
                                                                (potato.keyboard/really-focus (goog.dom/getElement "channel-input")))
                                                              (at-magic-close!)
                                                              true))
                                           :act-on-magic true})
              (potato.keyboard/set-event! editable potato.keyboard/ESC
                                          {:callback (fn []
                                                       (when-not (get-in (deref potato.state/global)
                                                                         [:channels cid :has-autocomplete-menu])
                                                         (potato.keyboard/destroy! editable)
                                                         (potato.keyboard/reset-cursor! keyboard-controller)
                                                         (om/set-state! owner :editing false)
                                                         (potato.keyboard/really-focus (goog.dom/getElement "channel-input")))
                                                       (at-magic-close!)
                                                       true)
                                           :act-on-magic true})
              (potato.keyboard/set-magic! editable "@" {:callback (fn [text event editable]
                                                                    (at-magic-callback owner text event editable :user))})
              (potato.keyboard/set-magic! editable ":" {:callback (fn [text event editable]
                                                                    (at-magic-callback owner text event editable :emoji))}))))
        om/IRenderState
        (render-state [_ {:keys [update-time editing previous-from previous-date]}]
          (let [user-entry (-> @potato.state/global :user-to-name-map (get (:from message)))]
            (om.dom/li
                #js {:id (str (:key-prefix opts) "-" (:id message))
                     :className (clojure.string/join " "
                                                     (remove nil?
                                                             [(if (and
                                                                   (== (:from message) previous-from)
                                                                   (<  (.diff (js/moment (:created_date message)) (js/moment previous-date)) 60000))
                                                                "chat-monologue")
                                                              (if (:highlighted-p opts)
                                                                "chat-highlighted")]))}
                (let [image (:image-name user-entry)]
                  (if image
                    (om.dom/img #js {:className "chat-author-picture"
                                     :src image})
                    ;; ELSE: Image information is not available yet, simply display an empty div
                    (om.dom/div #js {:className "chat-author-picture"})))
                (om.dom/figure #js {:className (str "chat-entry" (if (:unconfirmed message) " unconfirmed-message"))
                                    :data-id   (:hash message)}
                    (om.dom/figcaption nil
                        (om.dom/address #js {:className "chat-author"}
                                        (or (:description user-entry) ""))
                        (display-time (:created_date message)))
                    (om/build message-quote message {:state {:update-time      update-time
                                                             :editing          editing
                                                             :editable         (not (:unconfirmed message))
                                                             :editing-callback #(om/set-state! owner :editing %)}}))))))))

(defn roster-component [data owner]
  (reify
    om/IDisplayName (display-name [_] "roster-component")
    om/IRender
    (render [_]
      (om.dom/section
          #js {:id "roster"}
        (om.dom/h2 nil users-text)
        (apply om.dom/ul #js {:id "channel-online"}
               (om/build-all user-in-list
                             (sort-by #(clojure.string/lower-case (second %))
                                      (remove nil?
                                              (map (fn [[uid user-state]]
                                                     (let [user-name-entry (get (:user-to-name-map data) uid)]
                                                       (if (and user-name-entry (:active user-state))
                                                         (list uid (:description user-name-entry) (:active user-state)))))
                                                   (:users (get (:channels data) (:active-channel data))))))))
        (apply om.dom/ul #js {:id "channel-offline"}
               (om/build-all user-in-list
                             (sort-by #(clojure.string/lower-case (second %))
                                      (remove nil?
                                              (map (fn [[uid user-state]]
                                                     (let [user-name-entry (get (:user-to-name-map data) uid)]
                                                       (if (and user-name-entry (not (:active user-state)))
                                                         (list uid (:description user-name-entry) (:active user-state)))))
                                                   (:users (get (:channels data) (:active-channel data))))))))))))

(defn channel-toolbar [data owner]
  (reify
    om/IDisplayName (display-name [_] "channel-toolbar")
    om/IRender
    (render [_]
      (om.dom/aside #js {:id "toolbar"}
        (om/build potato.search/search-component data)
        (om/build roster-component data)))))

(defn merge-msg-update [msglist update-message]
  "Return an updated msglist where updates specificed in the update
  notification 'update' has been applied."
  (let [id (:id update-message)
        old-msg (get msglist id)]
    (if old-msg
      (let [index (update-index-of-message old-msg)]
        ;; We have to ensure that the update-index is increased from
        ;; the previous value. This value is located in the old
        ;; version of the message.
        (conj msglist {id (conj update-message {:update-index (+ index 1)})}))
      ;; ELSE: Message not found in list, return the original list
      msglist)))

(defn maybe-replace-draft [msglist new-message]
  "Return an updated msglist where the new-message replaces
   the unconfirmed messages with the same HASH it it exists.
   If the HASH is not found, just add the new-message to msglist."
  (let [new-hash (:hash new-message)
        new-list (loop [reversed-list (rseq msglist)]
                   ;; LOOP the list in reverse because the draft messages are at the end
                   (let [[_ message] (first reversed-list)]
                     (if (starts-with? (:id message) draft-marker)
                       ;; we found a draft message, check the hash
                       (if (= (:hash message) new-hash)
                         ;; we found a matching draft, remove it
                         (dissoc msglist (:id message))
                         ;; the draft didn't match our hash, move to the next one
                         (recur (rest reversed-list)))
                       ;; we did not found hash, no need to loop through the rest of the list
                       msglist)))]
    (assoc new-list (:id new-message) new-message)))

(defn- make-message-react-key [prefix msg]
  (let [i (update-index-of-message msg)]
    (str prefix "_" (:id msg) "_" i)))

(defn- build-message-view [messages-list state key-prefix highlighted-message]
  "Build the react object representing a list of messages.
messages-list - A list of message objects to be rendered
state - the state structure to be passed to the call to om/build
key-prefix - Used as a prefix when creating both the react key and the
             dom id. This is needed in order to support displaying
             the same message more than once in the dom tree.
highlighted-message - the message that should be highlighted (or
                      nil for no highlight)"
  (map
   (fn [[_ curmsg]]
     (let [message-highlighted-p (= (:id curmsg) highlighted-message)
           [_ prev] (first (rsubseq (deref messages-list) < (:id curmsg)))]
       (om/build message-view curmsg {:state (conj state {:previous-from (:from prev)
                                                          :previous-date (:created_date prev)})
                                      :react-key (make-message-react-key key-prefix curmsg)
                                      :opts {:highlighted-p message-highlighted-p
                                             :key-prefix key-prefix}})))
   messages-list))

(defn channel-history-range-splitter [cid]
  (om.dom/div #js {:className "channel-history-range-splitter"}
      (om.dom/a #js {:href "#"
                     :className "splitter-control"
                     :onClick (fn [_] (close-message-history-range cid))}
          "\u2715")
    (om.dom/span nil "Showing search results")))

(defn channel-history-range [range owner opts]
  (reify
      om/IDisplayName (display-name [_] "channel-history-range")
      om/IInitState   (init-state [_] {})
      om/IDidMount
      (did-mount [_]
        (let [highlighted-message-id (:message-id range)]
          (when highlighted-message-id
            ;; The "h" prefix for the message id comes from the
            ;; explicit string in the call to build-message-view
            ;; below.
            (let [element (.getElementById js/document (str "h-" highlighted-message-id))]
              (when element
                (.scrollIntoView element))))))
      om/IRenderState
      (render-state [_ _]
        (om.dom/div #js {:className "channel-history-range"}
            (om.dom/article nil
                (apply om.dom/ul nil
                       (build-message-view (:range-messages range) {:update-time false} "h" (:message-id range))))
          (channel-history-range-splitter (:channel-id opts))))))

(defn channel-history [messages owner opts]
  (let [channel-message-type :m]
    (reify
        om/IDisplayName (display-name [_] "channel-history")
        om/IInitState
        (init-state [_]
          {:history-loaded    false
           :messages-channel  (async/chan)
           :channel-scrolled  (async/chan (async/dropping-buffer 1))
           :update-time       false})
        om/IWillUnmount
        (will-unmount [_]
          (async/unsub (:event-publisher (om/get-shared owner))
                       channel-message-type
                       (om/get-state owner :messages-channel))
          (.clearInterval js/window (:interval (om/get-state owner))))
        om/IWillMount
        (will-mount [_]
          (let [channel-scrolled (om/get-state owner :channel-scrolled)]
            (go-loop []
              (async/<! channel-scrolled)
              (let [[min-id _]        (first @messages)
                    previous-messages (async/<! (fetch-messages min-id (:channel-id opts)))]
                (om/transact! messages []
                  #(reduce (fn [l v] (assoc l (:id v) (conj v {:client-top true}))) % previous-messages))
                (async/put! channel-scrolled true)
                (async/<! channel-scrolled))
              (recur)))
          (let [messages-chan (om/get-state owner :messages-channel)]
            (async/sub (:event-publisher (om/get-shared owner)) channel-message-type messages-chan)
            (go-loop []
              (if (om/get-state owner :history-loaded)
                (do
                  (let [msg-on-channel (last (async/<! messages-chan))]
                    (if (:updated msg-on-channel)
                      ;; The message is an update, modify the existing element
                      (om/transact! messages [] #(merge-msg-update % msg-on-channel))
                      ;; ELSE: Not an update, simply append the message
                      (om/transact! messages [] #(maybe-replace-draft % msg-on-channel)))
                    (recur)))
                ;; ELSE: History is not loaded yet
                (let [loaded-messages (async/<! (fetch-messages nil (:channel-id opts)))]
                  #_(when (empty? channel)
                      (async/<! (:initial-load-completed (om/get-shared owner))))
                  (om/set-state! owner :history-loaded true)
                  (om/transact! messages []
                    #(reduce (fn [l v] (assoc l (:id v) (conj v {:initial-load true}))) % loaded-messages))
                  (recur))))))
        om/IDidMount
        (did-mount [_]
          (let [node (om/get-node owner)
                channel-scrolled (om/get-state owner :channel-scrolled)]
            (goog.events/listen node goog.events/EventType.SCROLL (fn [xs] (if (== (.-scrollTop node) 0)
                                                                             (async/put! channel-scrolled xs)))))
          (om/set-state! owner :timer
                         (.setInterval js/window
                                       #(om/set-state! owner :update-time (not (:update-time (om/get-state owner)))) 60000)))
        om/IRenderState
        (render-state [_ {:keys [history-loaded update-time channel-scrolled]}]
          (om.dom/article nil
              (if history-loaded
                (apply om.dom/ul nil
                       (build-message-view messages {:update-time update-time} "m" nil))
                ;; ELSE: Display message stating that the history is being loaded
                loading-history))))))

(defn maybe-send-type-notification [typing-chan]
  (go-loop []
    (async/<! typing-chan)
    (let [current-domain  (:current-domain (deref (state-root)))
          active-channel  (:active-channel (deref (state-root)))
          ret (async/<! (http/post (str potato.urls/type-notification "/" (:id current-domain)
                                        "/" active-channel) {:json-params {}}))]
      (async/<! (async/timeout 3000))
      ;; clear the channel before recur
      (async/put! typing-chan true)
      (async/<! typing-chan)
      (recur))))

(defn- get-unique-id []
  (let [counter (om.core/ref-cursor (:unique-counter (om.core/root-cursor potato.state/global)))]
    (om.core/transact! counter [0] inc)
    (get counter 0)))

(defn encode-string-as-sha1 [s]
  "Encode a UTF-8 representation of the string s as a sequence of hex bytes."
  ;; Anyone looking at this thing might wonder why we're simply not
  ;; calling cljs-hash.goog/sha1-hex to do this work. Indeed this is
  ;; what was done previously, but it turns out that this
  ;; implementation is buggy and does not handle non-BMP characters
  ;; correctly.
  ;; See this bug report for details: https://github.com/google/closure-library/issues/470
  (let [digester (new js/goog.crypt.Sha1)]
    (.update digester (clj->js (map (fn [x] (.codePointAt x 0)) (js/unescape (js/encodeURIComponent s)))))
    (cljs.pprint/cl-format nil "~{~2,'0x~}" (.digest digester))))

(defn- make-draft-message [text-string html]
  (let [current-user (:current-user (deref potato.state/global))
        hash (encode-string-as-sha1 (str (:id current-user) "_" text-string))]
    {:id           (str draft-marker "draft:" (get-unique-id))
     :from         (:id current-user)
     :created_date (.format (.utc (js/moment)) "YYYY-MM-DDTHH:mm:ss.SSSZ")
     :text         text-string
     :raw_field    html
     :hash         hash
     :unconfirmed  true}))

(defn- send-with-retries [channel text-string html]
  (when (> (count text-string) 0)
    (let [draft-message (make-draft-message text-string html)]
      (om/transact! (om/ref-cursor channel) :messages #(assoc % (:id draft-message) draft-message))
      (go-loop [retries-counter 0]
        (let [{response :body}
              (async/<! (http/post potato.urls/send-chat {:json-params {:channel (:id channel) :text text-string}}))]
          (when (not (= (:result response) "ok"))
            (when (< retries-counter 10)
              (async/<! (async/timeout 3000))
              (recur (inc retries-counter)))
            (om/transact! (om/ref-cursor channel) :messages #(dissoc % (:id draft-message))))))
      true)))

(defn- process-slashcommand [cid text]
  (let [[_ cmd args] (re-matches #"^/([a-zA-Z0-9]+)(?: +(.*[^ ] *))?$" text)]
    (when cmd
      (http/post "/command" {:json-params {:channel cid
                                           :command cmd
                                           :arg (or args "")}}))))

(defn- autocomplete-find [key text]
  (condp = (type key)
    js/String  (not= -1 (.indexOf (clojure.string/lower-case key) text))
    cljs.core/PersistentVector (some #(not= -1 (.indexOf (clojure.string/lower-case %) text)) key)
    nil))

(defn autocomplete-menu [channel owner]
  (let [refresh-select-entry (fn []
                               (let [found-items (om/get-state owner :found-items)
                                     selected-index (om/get-state owner :selected-index)]
                                 (when (< selected-index (count found-items))
                                   (om.core/transact! channel :selected-menu-entry
                                                      #(nth found-items selected-index)))
                                 (when (= (count found-items) 0)
                                   (om.core/transact! channel :selected-menu-entry (fn [] nil)))))
        get-selected-entry (fn []
                             (let [found-items (om/get-state owner :found-items)
                                   selected-index (om/get-state owner :selected-index)]
                               (if (> (count found-items) 0)
                                 (if (>= selected-index (count found-items))
                                   nil
                                   (nth found-items selected-index)))))
        select-entry (fn [item]
                       (let [found-items (om/get-state owner :found-items)]
                         (when-let [indexes (keep-indexed (fn [idx itm] (when (= itm item) idx)) found-items)]
                           (om/set-state! owner :selected-index (first indexes))
                           (refresh-select-entry))))
        select-default-entry (fn []
                               (let [found-items (om/get-state owner :found-items)]
                                 (when (> (count found-items) 0)
                                   (om/set-state! owner :selected-index 0)
                                   (refresh-select-entry)
                                   (:selected-index @channel))))
        select-previous-entry (fn []
                                (let [found-items (om/get-state owner :found-items)
                                      selected-index (om/get-state owner :selected-index)]
                                  (if (> selected-index 0)
                                    (om/set-state! owner :selected-index (- selected-index 1))
                                    (if (> (count found-items) 0)
                                      (om/set-state! owner :selected-index (- (count found-items) 1))
                                      (om/set-state! owner :selected-index 0)))
                                  (refresh-select-entry)))
        select-next-entry (fn []
                            (let [found-items (om/get-state owner :found-items)
                                  selected-index (om/get-state owner :selected-index)]
                              (if (< selected-index (- (count found-items) 1))
                                (om/set-state! owner :selected-index (+ selected-index 1))
                                (om/set-state! owner :selected-index 0))
                              (refresh-select-entry)))]
    (reify
      om/IDisplayName (display-name [_] "autocomplete-menu")
      om/IInitState
      (init-state [_]
        {:found-items      []
         :selected-index   0
         :exit-move-chan   (async/chan)
         :exit-select-chan (async/chan)})
      om/IWillMount
      (will-mount [_]
        (let [exit-move-chan (om/get-state owner :exit-move-chan)
              exit-select-chan (om/get-state owner :exit-select-chan)]
          (go-loop []
            (let [[item-to-search channel] (async/alts! [(om/get-shared owner :search-item) exit-select-chan])]
              (when-not (= channel exit-select-chan)
                (let [lowercase-text (clojure.string/lower-case (:text item-to-search))
                      found-items (keep (fn [[k v]] (if (autocomplete-find v lowercase-text)
                                                      (case (:type item-to-search)
                                                        :user  {:id k :text v :special "user" :separator true}
                                                        :emoji {:id (potato.emoji/get-name v) :text k :special "emoji"}
                                                        nil)
                                                      nil))
                                        (case (:type item-to-search)
                                          :user  (update-values-map (:user-to-name-map @potato.state/global) :description)
                                          :emoji potato.emoji/unicode-to-aliases
                                          []))]
                  (let [selected-index (om/get-shared owner :selected-index)
                        items-count    (count found-items)]
                    (when (and (>= selected-index items-count) (> items-count 0))
                      (om/set-state! owner :selected-index (- items-count 1))))
                  (om/set-state! owner :found-items found-items)
                  (refresh-select-entry))
                (recur))))
          (go-loop []
            (let [[move-selected-menuitem channel] (async/alts! [(om/get-shared owner :move-selected-menuitem) exit-move-chan])]
              (when-not (= channel exit-move-chan)
                (case move-selected-menuitem
                  -1 (select-next-entry)
                  0  (select-default-entry)
                  1  (select-previous-entry))
                (recur))))))
      om/IWillUnmount
      (will-unmount [_]
        (async/put! (om/get-state owner :exit-select-chan) true)
        (async/put! (om/get-state owner :exit-move-chan) true))
      om/IRenderState
      (render-state [_ _]
        (when-let [has-autocomplete-menu (:has-autocomplete-menu channel)]
          (apply om.dom/menu #js {:id    "activeBehaviour"
                                  :style #js {:left     (:div-left  has-autocomplete-menu)
                                              :right    (:div-right has-autocomplete-menu)
                                              :bottom   (- (:viewport-height has-autocomplete-menu)
                                                           (:event-y         has-autocomplete-menu))
                                              :position "absolute"
                                              :display  "block"}}
                 (let [found-items (om/get-state owner :found-items)]
                   (if (> (count found-items) 0)
                     (let [selected (or (get-selected-entry) (select-default-entry))]
                       (map (fn [item]
                              (om.dom/menuitem #js {:className    (if (= (:id item) (:id selected)) "active")
                                                    :onMouseEnter #(select-entry item)
                                                    :onClick      #(potato.keyboard/key-on-default
                                                                    (om/get-shared owner :keyboard-control)
                                                                    potato.keyboard/ENTER)}
                                (if (= (:special item) "emoji") (potato.emoji/span item) (:text item))))
                            found-items))
                     [(om.dom/div #js {:className "notice"} "no match")]))))))))

(defn channel-input [channel owner]
  (reify
    om/IDisplayName (display-name [_] "channel-input")
    om/IInitState
    (init-state [_]
      {:typing-channel (async/chan)
       :typing         #{}})
    om/IWillUnmount
    (will-unmount [_]
      (async/unsub (:event-publisher (om/get-shared owner)) :type (om/get-state owner :typing-channel)))
    om/IDidMount
    (did-mount [this]
      (let [fieldset            (first (goog.dom/getElementsByTagNameAndClass "fieldset" nil (om/get-node owner)))
            keyboard-controller (om/get-shared owner :keyboard-control)
            typing-chan         (om/get-shared owner :typing-chan)
            editable            (potato.keyboard/append-editable-div {:parent-node      fieldset
                                                                      :keyboard         keyboard-controller
                                                                      :placeholder-text placeholder
                                                                      :typing-callback  #(async/put! typing-chan true)})]
        (potato.keyboard/set-event! editable potato.keyboard/ENTER
                                    {:callback (fn []
                                                 (when-not (:has-autocomplete-menu @channel)
                                                   (let [text (fixup-input-text (potato.keyboard/content editable))]
                                                     (if (and (not (empty? text))
                                                              (= (get text 0) "/"))
                                                       ;; Command was a slash command
                                                       (process-slashcommand (:id channel) text)
                                                       ;; ELSE: Send the message to the server
                                                       (send-with-retries channel
                                                                          text
                                                                          (potato.keyboard/innerHTML editable))))
                                                   (potato.keyboard/empty! editable)
                                                   (at-magic-close!)))
                                     :act-on-magic true})
        (potato.keyboard/set-event! editable potato.keyboard/ESC
                                    {:callback at-magic-close!})
        (potato.keyboard/set-magic! editable "@"
                                    {:callback (fn [text event editable]
                                                 (at-magic-callback owner text event editable :user))})
        (potato.keyboard/set-magic! editable ":"
                                    {:callback (fn [text event editable]
                                                 (at-magic-callback owner text event editable :emoji))}))
      (let [typing-channel (om/get-state owner :typing-channel)]
        (async/sub (:event-publisher (om/get-shared owner)) :type typing-channel)
        (go-loop []
          (let [typing-event (async/<! typing-channel)]
            (om/set-state! owner :typing (second typing-event))
            (recur)))))
    om/IRenderState
    (render-state [_ {typing :typing}]
      (om.dom/footer #js {:id "input"}
        (when (:has-autocomplete-menu channel)
          (om/build autocomplete-menu channel))
        (om.dom/fieldset nil "")
        (let [user-id (:id (:current-user (deref potato.state/global)))
              other-users-typing (disj typing user-id)]
          (if (> (count other-users-typing) 0)
            (om.dom/div #js {:className "typing"}
              (cljs.pprint/cl-format nil is-or-are-typing
                                     (map (fn [uid]
                                            (:description (get (:user-to-name-map @potato.state/global) uid)))
                                          other-users-typing)))))))))

(defn channel-view [channel owner opts]
  (reify
      om/IDisplayName (display-name [_] "channel-view")
      om/IInitState
      (init-state [_]
        {:uploader nil
         :dnd      nil})
      om/IDidMount
      (did-mount [_]
        (om/set-state! owner
                       :uploader (potato.fineuploader/create-fine-uploader #(:uploader (om/get-state owner))
                                                                           (:id (:current-user (deref potato.state/global))) (:id channel)))
        (om/set-state! owner
                       :dnd      (potato.fineuploader/enable-drag-and-drop (:uploader (om/get-state owner)) #(println "called back")))
        (request-user-list-for-channel (:id channel)))
      om/IWillUnmount
      (will-unmount [_]
        (let [dnd (:dnd (om/get-state owner))]
          (if dnd (.dispose dnd))))
      om/IRender
      (render [_]
        (let [cid (:id channel)]
          (apply om.dom/section #js {:className "channel"}
                 (concat
                  (list (om.dom/div #js {:id "upload-progress"}
                            (om.dom/div #js {:id "show-progress"} nil)
                            (om.dom/div #js {:id "uploading-text"} uploading-file-text)
                            (om.dom/div #js {:id "uploading-percent"} "0%")
                            (om.dom/a   #js {:id "uploading-cancel" :href "#"} nil))
                        (om/build channel-header  channel))
                  (let [range (:range channel)]
                    (when range
                      (list (om/build channel-history-range range {:opts {:channel-id cid}
                                                                   :react-key (str "channel-history-" cid)}))))
                  (list (om/build channel-history (:messages channel) {:opts {:channel-id cid}
                                                                       :react-key (str "channel-content-" cid)})
                        (om/build channel-input channel))))))))

(defn connection-state-build-root [conn owner]
  (reify
    om/IDisplayName
    (display-name [_]
      "connection-status")
    om/IRender
    (render [_]
      (when (:error conn)
        (om.dom/div #js {:className "active" :id "warning"} "Server connection failed")))))

(defn potato [app owner]
  (reify
    om/IDisplayName (display-name [_] "potato")
    om/IInitState
    (init-state [_]
      (print "Init state of potato")
      {:notifications-enabled false
       :hide-alerts false
       :preferences-open false
       :disabled-input []})
    om/IWillMount
    (will-mount [_]
      (if (and (.hasOwnProperty js/window "Notification") (= (.-permission js/Notification) "granted"))
        (om/set-state! owner :notifications-enabled true))
      (go-loop []
        (let [new-state (async/<! (:preferences-chan (om/get-shared owner)))]
          (om/set-state! owner :preferences-open new-state)
          ;; FIXME: disable Drag-and-drop as well
          (if new-state
            (potato.keyboard/disable (om/get-shared owner :keyboard-control))
            (potato.keyboard/enable  (om/get-shared owner :keyboard-control)))
          (recur))))
    om/IRenderState
    (render-state [this {:keys [notifications-enabled hide-alerts preferences-open]}]
      (let [current-channel (get-in app [:channels (:active-channel app)])]
        (when current-channel
          (om.dom/div #js {:id "potato"}
            (when (and (not notifications-enabled) (not hide-alerts))
              (om.dom/div #js {:id "potato-alert"} needs-your-permission-text nonbreak-space
                          (om.dom/a #js {:className "enable"
                                         :onClick   (fn [e] (potato.preferences/open-screen owner true)
                                                      (.preventDefault e))}
                            enable-notifications-text)
                          "."
                          (om.dom/a #js {:className "dismiss"
                                         :onClick   #(om/set-state! owner :hide-alerts true)} "")))
            (om.dom/main #js {:id "potato-core"}
                         (om/build channels-list app)
                         (om/build channel-view current-channel)
                         (om/build channel-toolbar app))
            ;; add a div to capture clicks
            (when preferences-open
              (om.dom/div #js {:id        "click-mask"
                               :className "capture"
                               :onClick   (fn [e] (potato.preferences/open-screen owner false)
                                            (.preventDefault e))} ""))
            (when preferences-open
              (om/build potato.preferences/preferences app))))))))

(defn switch-channel [new-channel-id]
  (om.core/transact! (om.core/root-cursor potato.state/global)
                     [:active-channel]
                     #(str new-channel-id))
  (potato.eventsource2/start-notification-handler new-channel-id)
  (request-channel-info new-channel-id)
  (if (goog.history.Html5History/isSupported)
    (let [history (goog.history.Html5History.)]
      (.setUseFragment history false)
      (.setPathPrefix  history potato.urls/channel-root)
      (.setToken       history (str "/" new-channel-id)))))

(defn main []
  (println (str "-- main called at " (.format (js/moment))))
  (let [event-channel    (make-polling-connection (:active-channel (deref potato.state/global)))
        event-publisher  (async/pub event-channel (fn [[tag & _]] tag))
        root-node        (goog.dom/getElement "potato-root")
        connection-state-info-node (goog.dom/getElement "connection-state")
        preferences-chan (async/chan (async/dropping-buffer 1))
        typing-chan      (async/chan (async/dropping-buffer 1))
        initial-load-completed (async/chan (async/dropping-buffer 1))]
    (request-channel-info-for-all-channels
     #(do
        (async/put! initial-load-completed true)
        ;; TODO: The channel info that is returned from
        ;; request-channel-info-for-all-channels does not contain some
        ;; information (like the topic). Thus, we need to make sure
        ;; that request-channel-info is called after the previous call
        ;; has returned.
        (request-channel-info (:active-channel @potato.state/global))))
    (goog.events/listen (goog.ui.IdleTimer. 0) (.-BECOME_ACTIVE goog.ui.IdleTimer/Event) mark-notifications)
    (maybe-send-type-notification typing-chan)

    (om/root potato
             potato.state/global
             {:shared {:event-channel          event-channel
                       :event-publisher        event-publisher
                       :root-node              root-node
                       :has-autocomplete-menu  nil
                       :typing-chan            typing-chan
                       :search-item            (async/chan (async/dropping-buffer 1))
                       :move-selected-menuitem (async/chan (async/dropping-buffer 1))
                       :preferences-chan       preferences-chan
                       :keyboard-control       (potato.keyboard/init root-node)
                       :initial-load-completed initial-load-completed}
              :target root-node})

    (om/root connection-state-build-root
             potato.state/connection
             {:target connection-state-info-node})))
