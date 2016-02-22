(ns potato.state)

(defonce init-channel-id       (aget js/window "channelId"))
(defonce init-user-id          (aget js/window "userId"))
(defonce init-user-description (aget js/window "userDescription"))
(defonce init-admin-p          (aget js/window "isAdmin"))
(defonce init-domain-id        (aget js/window "domainId"))
(defonce init-domain-name      (aget js/window "domainName"))
(defonce init-s3-credentials   (js->clj (aget js/window "s3Credentials") :keywordize-keys true))

(defonce global (atom {:channels         {}
                       :unique-counter   [0]
                       :active-channel   init-channel-id
                       :current-domain   { :id init-domain-id :name init-domain-name }
                       :current-user     { :id init-user-id   :name init-user-description
                                          :is-admin? (if init-admin-p true false) }
                       ;; Mapping between user ID's and description and image.
                       ;; The key is the user id, and the value is a map of the following form:
                       ;; {:description "name" :image-name "foo.jpg"}
                       :user-to-name-map {}
                       :outstanding-notifications []
                       :session-id (str (random-uuid))}))

(defonce connection (atom {:error false}))
