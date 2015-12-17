(ns potato.admin.group-edit
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

(defonce state (atom {:available-groups []  ; List of groups
                      :edit nil             ; Current edited data
                      }))

(defonce init-domain-id (aget js/window "domainId"))

(defn- request-group-list []
  (go (let [{msg :body} (async/<! (http/post "/admin/find-group-list" {:json-params {:domain init-domain-id}}))]
        (om/transact! (om/root-cursor state) [:available-groups]
          (fn [_]
            (sort-by :description
                     (map (fn [d] {:id (:id d)
                                   :description (:description d)})
                          (:groups msg))))))))

(defn- add-user-to-group [gid uid user-description]
  (cljs.pprint/cl-format true "Adding ~s to ~s" uid gid)
  (go (let [{msg :body} (async/<! (http/post "/admin/update-group" {:json-params {:cmd "add-user"
                                                                                  :group gid
                                                                                  :user uid
                                                                                  :role "USER"}}))]
        (when (= (:result msg) "ok")
          (om/transact! (om/root-cursor state) [:edit :users]
            (fn [users]
              (conj users {uid  {:description user-description :role :user}})))))))

(defn- remove-user-from-group [gid uid]
  (go (let [{msg :body} (async/<! (http/post "/admin/update-group" {:json-params {:cmd "remove-user"
                                                                                  :group gid
                                                                                  :user uid}}))]
        (when (= (:result msg) "ok")
          (om/transact! (om/root-cursor state) [:edit :users] #(dissoc % uid))))))

(defn- role-symbol->string [role]
  (case role
    :user "USER"
    :admin "ADMIN"
    (throw (cljs.pprint/cl-format false "Illegal role: ~s" role))))

(defn- role-string->symbol [role]
  (case role
    "USER" :user
    "ADMIN" :admin
    (throw (cljs.pprint/cl-format false "Illegal role: ~s" role))))

(defn- update-role [gid uid role]
  "Updates the role of the specific user in a group. The user must already be a member of the group."
  (go (let [{msg :body} (async/<! (http/post "/admin/update-group" {:json-params {:cmd "update-role"
                                                                                  :group gid
                                                                                  :user uid
                                                                                  :role (role-symbol->string role)}}))]
        (when (= (:result msg) "ok")
          (om/transact! (om/root-cursor state) [:edit :users uid]
            (fn [user]
              (when (not user)
                (throw "User is not in member list"))
              (conj user {:role role})))))))

(defn- create-channel [gid name]
  (go (let [{msg :body} (async/<! (http/post "/admin/create-channel" {:json-params {:group gid
                                                                                    :name name}}))]
          (when (= (:result msg) "ok")
            (om/transact! (om/root-cursor state) [:edit :channels]
              (fn [channels]
                (conj channels {(:id msg)
                                {:name (:name msg)}})))))))

(defn- parse-group-data [msg]
  "Transforms the group data as returned from the server into the proper internal format."
  (cljs.pprint/cl-format true "parsing: ~s" msg)
  {:id (:id msg)
   :description (:description msg)
   :users (reduce (fn [l user]
                    (conj l
                          {(:id user)
                           {:description (:description user)
                            :role (role-string->symbol (:role user))}}))
                  {} (:users msg))
   :channels (reduce (fn [l channel]
                       (conj l
                             {(:id channel)
                              {:name (:name channel)}}))
                     {} (:channels msg))})

(defn- parse-search-result [result]
  (map (fn [v]
         {:id (:id v)
          :description (:description v)})
       (:users result)))

(defn- group-editor-user-selector [data owner]
  "Renders the left panel for the group editor. This panel contains a
search input field for the user name, a list of search results and an
'add' button."
  (reify
      om/IDisplayName (display-name [_] "group-editor-user-selector")

      om/IInitState
      (init-state [_]
        {:search-channel (async/chan (async/dropping-buffer 1))
         :search-results []})

      om/IWillMount
      (will-mount [_]
        (let [channel (om.core/get-state owner :search-channel)]
          (go-loop []
            (let [search-text (async/<! channel)]
              (when (and search-text (>= (count search-text) 2))
                (let [{result :body} (async/<! (http/post "/userlist-search" {:json-params {:domain init-domain-id
                                                                                            :query search-text}}))]
                  (cljs.pprint/cl-format true "Search results: ~s" result)
                  (om.core/set-state! owner :search-results (sort-by :description (parse-search-result result)))))
              (recur)))))

      om/IRenderState
      (render-state [_ {}]
        (om.dom/div #js {:className "group-editor-left-panel"}
          ;; The search input panel contains the label and the input field
          (om.dom/div #js {:className "group-editor-search-input-panel"}
            "User name:"
            (om.dom/input #js {:id "group-editor-search-field"
                               :placeholder "Search user"
                               :onKeyUp (fn [event]
                                          (async/put! (om.core/get-state owner :search-channel)
                                                      (goog.dom.forms/getValue (.-currentTarget event)))
                                          (.preventDefault event))
                               }))
          ;; The search result panel contains the list of matching usernames
          (om.dom/div #js {:className "group-editor-search-result-panel"}
            (let [search-results (om.core/get-state owner :search-results)]
              (if search-results
                (apply om.dom/ul nil
                       (map (fn [user]
                              (om.dom/li #js {:className "group-editor-search-results-entry"
                                              :onClick (fn [event]
                                                         (add-user-to-group (:id data) (:id user) (:description user)))}
                                (:description user)))
                            search-results))
                ;; ELSE: No search results
                "No results")))))))

(defn- make-group-editor-row [uid user-info gid]
  "Create a table row for the user list table."
  (om.dom/tr nil
    (om.dom/td nil (:description user-info))
    (om.dom/td nil
      (om.dom/input #js {:type "checkbox"
                         :checked (= (:role user-info) :admin)
                         :onChange (fn [event] (update-role gid uid (if (.-checked (.-currentTarget event)) :admin :user)))}))
    (om.dom/td nil
      (om.dom/a #js {:href "#"
                     :className "group-editor-table-button"
                     :onClick (fn [_] (remove-user-from-group gid uid))}
        "\u2715"))))

(defn- users-panel [data]
  (om.dom/div #js {:className "group-editor-users"}
    ;; Left panel
    (om/build group-editor-user-selector data)
    ;; Right panel
    (om.dom/div #js {:className "group-editor-right-panel"}
      (om.dom/div #js {:className "group-editor-user-list-title"}
        "Users in group")
      (om.dom/table #js {:className "group-editor-user-list"}
        (om.dom/thead nil
          (om.dom/tr nil
            (om.dom/th nil "Name")
            (om.dom/th nil "Admin")
            (om.dom/th nil "Remove")))
        (apply om.dom/tbody nil
               (map (fn [[uid user-info]]
                      (make-group-editor-row uid user-info (:id data))) (:users data)))))))

(defn- add-channel-panel [data owner]
  (reify
      om/IDisplayName (display-name [_] "add-channel-panel")
      om/IInitState (init-state [_] {:name ""})
      om/IRenderState
      (render-state [_ values]
        (om.dom/div #js {:className "group-editor-channel-list-add-channel"}
          (om.dom/span nil "Create channel:")
          (om.dom/input #js {:type "text"
                             :value (:name values)
                             :onChange #(om/set-state! owner :name (goog.dom.forms/getValue (.-currentTarget %)))})
          (om.dom/button #js {:onClick (fn [_]
                                         (create-channel (:id data) (:name values))
                                         (om/set-state! owner :name ""))}
                         "Create")))))

(defn- channel-list-panel [data]
  (om.dom/div #js {:className "group-editor-channel-list-panel"}
    (om.dom/div #js {:className "group-editor-channel-list-title"} "Channels")
    (apply om.dom/ul #js {:className "group-editor-channel-list-content"}
           (map (fn [[id channel-info]]
                  (om.dom/li nil
                    (:name channel-info)))
                (:channels data)))
    (om/build add-channel-panel data)))

(defn- group-editor [data owner]
  (reify
      om/IDisplayName (display-name [_] "group-editor")
      om/IRender
      (render [_]
        ;; The group editor is supposed to look something like this:
        ;;
        ;;  ------------------------------------------------------
        ;;  |       -------------   | Users in group             |
        ;;  | Name: | Search    |   |                            |
        ;;  |       -------------   |     Name    Admin  Remove  |
        ;;  |   ------------------- |   ------------------------ |
        ;;  |   | Name 1          | |     Name 1   [X]     X     |
        ;;  |   | Name 2          | |     Name 2   [ ]     X     |
        ;;  |   | Name 3          | |                            |
        ;;  |   | Name 4          | |                            |
        ;;  |   ------------------- |                            |
        ;;  |  -----                |                            |
        ;;  |  |Add|                |                            |
        ;;  |  -----                |                            |
        ;;  ------------------------------------------------------
        ;;  | Channels                                           |
        ;;  |                                                    |
        ;;  |   Channel 1                                        |
        ;;  |   Channel 2                                        |
        ;;  |                                                    |
        ;;  | Create channel: [________________] [Add]           |
        ;;  |                                                    |
        ;;  ------------------------------------------------------
        (om.dom/div #js {:className "group-editor"}
          ;; Top panel containing the rights assignments
          (users-panel data)
          ;; Bottom panel containing the channel list
          (channel-list-panel data)))))

(defn- select-group [gid]
  (go (let [{msg :body} (async/<! (http/post "/admin/load-group" {:json-params {:group gid}}))]
        (let [data (parse-group-data (:group msg))]
          (om/transact! (om/root-cursor state) []
            (fn [group]
              (conj group {:edit data})))))))

(defn- render-group-name [available-group]
  (om.dom/a #js {:href "#"
                 :onClick (fn [_] (select-group (:id available-group)))}
    (cljs.pprint/cl-format false "Group ~a" (:description available-group))))

(defn- group-admin-panel [group-admin-state owner]
  "View function for the group admin panel. This panel contains a list
of all groups the current user is allowed to edit, and a panel where
the currently selected group can be edited."
  (reify
      om/IDisplayName (display-name [_] "group-admin-panel")
      om/IRender
      (render [_]
        (om.dom/div nil
          (om.dom/div nil
            "Select group to edit"
            (apply om.dom/div {:id "group-selector"} (map render-group-name (:available-groups group-admin-state))))
          (if (:edit group-admin-state)
            (om.dom/div nil
              "This is the editor for the selected group"
              (om/build group-editor (:edit group-admin-state)))
            ;; ELSE: No group has been selected yet. Probably the edit panel shouldn't be displayed at all.
            (om.dom/div nil "No group selected"))))))

(defn group-admin [app owner]
  (reify
      om/IDisplayName (display-name [_] "group-admin")
      om/IInitState (init-state [_] {})
      om/IWillMount (will-mount [_] (request-group-list))
      om/IRenderState
      (render-state [this _]
        (om.dom/div nil
          (om/build group-admin-panel app)))))

(defn group-admin-main []
  (let [root-node (goog.dom/getElement "group-admin")]
    (om/root group-admin
             state
             {:shared {}
              :target root-node})))
