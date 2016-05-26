;;; sec:init

(ns potato.eventsource2
  (:require [cljs-http.client :as http]
            [cljs.core.async  :as async]
            [cljs.pprint]
            [goog.net.XhrIo]
            [goog.net.EventType]
            [potato.urls])
  (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]]))

(defonce active-config (atom {:callback-fn #()
                              :error-fn #()
                              :error-p false
                              :is-active true}))

(defonce current-index (atom 0))

;;; sec:util

(defn swapold [atom f & args]
  (loop []
    (let [old @atom
          new (apply f old args)]
      (if (compare-and-set! atom old new)
        [old new]
        (recur)))))

(defn current-time []
  (.getTime (new js/Date)))

(defn find-next-index []
  (swap! current-index inc))

(defn notify-fail [error-p]
  (let [[old new] (swapold active-config #(conj % {:error-p error-p}))
        old-error (:error-p old)]
    (when (or (and old-error (not error-p))
              (and (not old-error) error-p))
      (cljs.pprint/cl-format true "Calling error handler function with state: ~s" error-p)
      ((:error-fn @active-config) error-p))))

(defn set-is-active [active-p]
  (swap! active-config #(conj % {:is-active active-p})))

;;; sec:poll

(defonce initial-poll-config {:channel nil
                              :event nil
                              :error false})
(defonce poll-config (atom initial-poll-config))

(defn handle-poll-error []
  (notify-fail true)
  (swap! poll-config #(conj % {:error true})))

(defn run-polling-request-loop [req]
  (if (:error @poll-config)
    (.setTimeout js/window
                 (fn []
                   (swap! poll-config #(conj % {:error false}))
                   (notify-fail false)
                   (run-polling-request-loop req))
                 5000)
    ;; ELSE: No error, request the next event
    (.send req "/chat_updates6" "POST"
           (.stringify js/JSON (clj->js {:channel (:channel @poll-config)
                                         :connection (:event @poll-config)
                                         :session-id (:session-id @potato.state/global)
                                         :is-active (:is-active @active-config)})))))

(defn make-xhr-io-req []
  (let [req (new goog.net.XhrIo)]
    (goog.events.listen req goog.net.EventType/COMPLETE
      (fn [event]
        (this-as ref
          (case (.getStatus ref)
            ;; Handle new event
            200
            (let [res (js->clj (.getResponseJson ref) :keywordize-keys true)]
              (if (= (:result res) "error")
                ;; Error result from server
                (if (= (:detail res) "unknown_event")
                  (do
                    (aset js/window "location" "/")
                    (throw "Long poll returned 'unknown_event' result"))
                  (handle-poll-error))
                ;; ELSE: Valid result
                (let [queue (:connection res)]
                  (doseq [msg (:data res)]
                    ((:callback-fn @active-config) msg))
                  (swap! poll-config #(conj % {:event queue})))))
            ;; Permission error, go back to main page
            401
            (do
              (aset js/window "location" "/")
              (throw "Permission error when getting events"))
            ;; ELSE: HTTP error
            (handle-poll-error)))))
    (goog.events.listen req goog.net.EventType/READY
      (fn [event]
        (run-polling-request-loop req)))
    (goog.events.listen req goog.net.EventType/ERROR
      (fn [event]
        (cljs.pprint/cl-format true "Poll error")))
    (goog.events.listen req goog.net.EventType/ABORT
      (fn [event]
        (cljs.pprint/cl-format true "Poll abort")))
    (goog.events.listen req goog.net.EventType/TIMEOUT
      (fn [event]
        (cljs.pprint/cl-format true "Poll timeout")))

    req))

(defn start-poll [cid event]
  (let [req (make-xhr-io-req)]
    (notify-fail false)
    (swap! poll-config #(conj % {:channel cid :event event}))
    (run-polling-request-loop req)))

;;; sec:ws

(defonce initial-websocket-config {:channel nil
                                   :event nil
                                   :last-index nil
                                   :last-refresh-send-time nil
                                   :refresh-reply-received false})
(defonce websocket-config (atom initial-websocket-config))

;; Forward declarations
(declare start-websocket)

(defn handle-websocket-timeout [ws]
  (cljs.pprint/cl-format true "Websocket ping reply timeout")
  (.close ws))

(defn handle-websocket-refresh [ws msg]
  (let [received-index (:data msg)
        last-index (:last-index @websocket-config)]
    (when (not= received-index last-index)
      (throw (cljs.pprint/cl-format false "Wrong index from server. Got ~s, expected ~s" received-index last-index)))
    (.clearTimeout js/window (:timer @websocket-config))
    (swap! websocket-config #(conj % {:timer nil :refresh-reply-received true}))))

(defn start-websocket-ping [ws]
  (when (= (.-readyState ws) (.-OPEN js/WebSocket))
    (let [now (current-time)
          index (find-next-index)]
      (.send ws (.stringify js/JSON (clj->js {:cmd "refresh" :data index :is-active (:is-active @active-config)})))
      (let [timer (.setTimeout js/window (fn [] (handle-websocket-timeout ws)) 5000)
            next-ping-timer (.setTimeout js/window (fn [] (start-websocket-ping ws)) 30000)]
        (swap! websocket-config #(conj % {:last-index index
                                          :last-refresh-send-time now
                                          :timer timer
                                          :next-ping-timer next-ping-timer}))))))

(defn handle-websocket-message [ws msg]
  (case (:type msg)
    "event" (swap! websocket-config #(conj % {:event (:event msg)}))
    "refresh" (handle-websocket-refresh ws msg)
    ;; Pass all other messages to the callback function
    ((:callback-fn @active-config) msg)))

(defn handle-websocket-close [ws]
  (let [cancel-if-active (fn [timer]
                           (when timer
                             (.clearTimeout js/window timer)))]
    ;; Cancel all outstanding timers
    (cancel-if-active (:timer @websocket-config))
    (cancel-if-active (:next-ping-timer @websocket-config))
    ;; Remove the websocket instance and timers from the active config
    (swap! websocket-config #(conj % {:connection nil :timer nil :next-ping-timer nil}))
    ;; Attempt a reconnect, using websocket or try a different method
    (if (:refresh-reply-received @websocket-config)
      ;; If the connection was working, simply attempt to reconnect
      (start-websocket (:channel @websocket-config) (:event @websocket-config))
      ;; ELSE: No ping reply had been received before the disconnect, fall back to long poll
      (start-poll (:channel @websocket-config) (:event @websocket-config)))))

(defn start-websocket [cid initial-event]
  (cljs.pprint/cl-format true "Starting websocket for channel: ~s, event: ~s, on: ~s" cid initial-event (aget js/window "websocketUrl"))
  (let [ws (new js/WebSocket (cljs.pprint/cl-format false "~a/~a?session_id=~a~@[&event=~a~]"
                                                    (aget js/window "websocketUrl")
                                                    cid
                                                    (:session-id @potato.state/global)
                                                    initial-event))]
    (.addEventListener ws "message"
                       (fn [event]
                         (handle-websocket-message ws (js->clj (js/JSON.parse (aget event "data")) :keywordize-keys true))))
    (.addEventListener ws "open"
                       (fn [event]
                         (cljs.pprint/cl-format true "Websocket event: open")
                         (notify-fail false)
                         (start-websocket-ping ws)))
    (.addEventListener ws "close"
                       (fn [event]
                         (cljs.pprint/cl-format true "Websocket event: close")
                         (handle-websocket-close ws)))
    (.addEventListener ws "error"
                       (fn [event]
                         (cljs.pprint/cl-format true "Websocket event: error")
                         (notify-fail true)
                         (.close ws)))
    (swap! websocket-config (fn [config]
                              (conj config {:refresh-reply-received false
                                            :channel cid
                                            :event initial-event
                                            :connection ws})))))

(defn set-callback-fn! [callback-fn]
  (swap! active-config #(conj % {:callback-fn callback-fn})))

(defn set-error-fn! [error-fn]
  (swap! active-config #(conj % {:error-fn error-fn})))

(defn start-notification-handler [cid]
  (start-websocket cid nil)
  ;; This function can be used to force polling instead of trying eventsocket
  #_(start-poll cid nil))
