;; ClojureScript by Mathieu Legrand <mathieu@legrand.im>
(ns potato.keyboard
  (:require  [clojure.string]
             [goog.dom]
             [goog.style]
             [goog.events]
             [goog.dom.xml]
             [goog.dom.classlist]
             [goog.dom.selection]
             [goog.testing.events]
             [goog.testing.dom]
             [goog.events.EventTarget]
             [goog.events.KeyHandler]
             [goog.events.PasteHandler]
             [goog.events.BrowserEvent]
             [goog.dom.Range]
             [goog.editor.node]
             [goog.editor.range]
             [goog.Uri]))

(def ENTER 13)
(def ESC   27)
(def TAB    9)
(def UP    38)
(def DOWN  40)

(def MARKER "\uDB80\uDC01")

(defn is-TAB?   [event] (= (.-keyCode event) TAB))
(defn is-ENTER? [event] (= (.-keyCode event) ENTER))
(defn is-ESC?   [event] (= (.-keyCode event) ESC))
(defn is-UP?    [event] (= (.-keyCode event) UP))
(defn is-DOWN?  [event] (= (.-keyCode event) DOWN))

(defn has-keyboard []
  "test whether ?keyboard=1 is set as an URL parameter"
  (let [param (clojure.string/lower-case (or (.getParameterValue (goog.Uri/parse js/location) "keyboard") ""))]
    (or (= param "1") (= param "yes") (= param "true"))))

(declare get-parsed-contend)

(defn- siblings-loop [child]
  (when child
    (str (get-parsed-contend child)
         (if-let [nextNode (.-nextSibling child)]
           (siblings-loop nextNode)))))

(defn- transform-special-tag [node]
  (let [special        (.getAttribute node "potato-special")
        id             (.getAttribute node "potato-id")
        value          (case (.-nodeName node)
                         "INPUT"  (.-value node)
                         "BUTTON" (goog.dom/getRawTextContent node)
                         "")]
    (case special
      "user"  (str MARKER special ":" id ":" value MARKER)
      "emoji" value
      (str MARKER special ":" id ":" value MARKER))))

(defn get-parsed-contend [node]
  (case (.-nodeName node)
    "SCRIPT" nil
    "STYLE"  nil
    "HEAD"   nil
    "IFRAME" nil
    "OBJECT" nil
    "BR"     "\n"
    "DIV"    (str "\n" (siblings-loop (.-firstChild node)))
    "IMG"    " "
    "INPUT"  (transform-special-tag node)
    "BUTTON" (transform-special-tag node)
    (condp = (.-nodeType node)
      (.-TEXT goog.dom/NodeType) (.-nodeValue node)
      (siblings-loop (.-firstChild node)))))

(defn create-uneditable-container [{:keys [id text special separator]}]
  (let [element (if goog.userAgent.GECKO
                  (goog.dom/createDom "input"  #js {:type            "button"
                                                    :disabled        "disabled"
                                                    :value           text})
                  (goog.dom/createDom "button" #js {:contentEditable false} text))]
    (goog.dom.classlist/add element (case special
                                      "user"  "at"
                                      "emoji" "emoji"
                                      ""))
    (goog.dom.xml/setAttributes element #js {:potato-special special
                                             :potato-id      id})
    (if-not goog.userAgent.GECKO
      (goog.style/setStyle element #js {:user-input "disabled"}))
    {:element element :separator separator}))

(defn- process-magic [text]
  (let [alist   (clojure.string/split text ":")
        special (first alist)
        id      (second alist)
        value   (last   alist)]
    (if-let [container (create-uneditable-container {:id id :text value :special special})]
      (:element container))))

(defn- process-text [text]
  (let [alist (clojure.string/split text "\n")]
    (let [result (flatten (map (fn [x] [x (goog.dom/createElement "br")]) alist))]
      result)))

(defn join-and-parse-as-html [coll result]
  (case (count coll)
    0 (conj result goog.string.Unicode.NBSP)            ;; LAST element after <MAGIC>, add NBSP for cursor
    1 (apply conj result (process-text (first coll)))   ;; SIMPLE TEXT
    (recur (nthnext coll 2) (conj result (first coll) (process-magic (second coll))))))

(defn get-html-from-potato-text [text]
  (join-and-parse-as-html (clojure.string/split text MARKER) []))

(defn content [editable]
  (if-let [node (:div @editable)]
    (get-parsed-contend node)))

(defn innerHTML [editable]
  (if-let [node (:div @editable)]
    (.-innerHTML node)))

(defn empty! [editable]
  (goog.dom/setTextContent (:div @editable) "")
  true)

(defn destroy! [editable]
  (swap! editable assoc :active false)
  (when (:placeholder @editable)
    (goog.dom/removeNode (:placeholder @editable)))
  (goog.dom/removeNode (:div @editable))
  (swap! (:keyhandler @editable) update-in [:editables-list] (fn [alist] (remove #(= editable %) alist))))

(defn set-placeholder! [editable placeholder-text]
  (when (:active @editable)
    (when (and (not (:placeholder @editable)) placeholder-text)
      (let [editable-div (:div @editable)
            placeholder  (goog.dom/createDom "span" #js {:className "placeholder"
                                                         :onClick   #(.focus editable-div)} placeholder-text)]
        (when-not (> (count (content editable)) 0)
          (goog.dom/insertSiblingBefore placeholder editable-div)
          (swap! editable assoc :placeholder placeholder)))
      (js/setTimeout #(set-placeholder! editable placeholder-text) 10000))))

(defn reset-cursor! [keyhandler & [editable]]
  "Place cursor in editable (or default) at the end of the content"
  (let [editable-div    (or editable (:div (deref (:default-editable @keyhandler))))
        cursor-position (goog.editor.node/getRightMostLeaf editable-div)]
    (if (= cursor-position editable-div)
      (.select (goog.dom.Range/createCaret editable-div 0))
      (goog.editor.range/placeCursorNextTo cursor-position false))))

(defn really-focus [mydiv & {:keys [timeout] :or [timeout 100]}]
  "Really focus a given DIV by calling the Javascript focus now and after a timeout"
  (.focus mydiv)
  (js/setTimeout #(.focus mydiv) timeout))

(defn append-editable-div [opts]
  "append an editable div channel-input within the :parent-node DOM element"
  ;; e.g. (append-editable-div {:id "channel-input"
  ;;                            :keyboard (potato.keyboard/init dom-body)
  ;;                            :text-content "Default content"
  ;;                            :parent-node dom-node})
  (let [keyhandler   (:keyboard opts)
        contents     (when (> (count (:text-content opts)) 0) (get-html-from-potato-text (:text-content opts)))
        editable-div (apply goog.dom/createDom "div" (clj->js {:id              (or (:id opts) "channel-input")
                                                               :spellcheck      (or (:spellcheck opts) true)
                                                               :contentEditable true
                                                               :role            "textbox"}) contents)]
    (goog.dom/append (:parent-node opts) editable-div)
    (let [paste-event-handler (goog.events.PasteHandler. editable-div)]
      (goog.events/listen paste-event-handler (.-PASTE goog.events.PasteHandler/EventType)
                          (fn [event] (.preventDefault event)
                            (let [clipboard-text (.getData (.-clipboardData (.getBrowserEvent event)) "text/plain")]
                              (.execCommand js/document "insertText" false clipboard-text)))))
    (really-focus editable-div)
    (reset-cursor! keyhandler editable-div)
    (let [editable-definition (atom {:active            true
                                     :locked            false
                                     :saved-opts        opts
                                     :keyhandler        keyhandler
                                     :div               editable-div
                                     :typing-callback   (:typing-callback opts)
                                     :new-size-callback (:new-size-callback opts)
                                     :previous-height   (.-height (goog.style/getSize editable-div))})]
      (when (:placeholder-text opts)
        (set-placeholder! editable-definition (:placeholder-text opts)))
      (if (= (count (:editables-list @keyhandler)) 0)
        (swap! keyhandler assoc :default-editable editable-definition))
      (swap! keyhandler update-in [:editables-list] #(conj % editable-definition))
      editable-definition)))

(defn set-event! [editable event event-definition]
  "Add a callback for pre-defined event on the given editable"
  ;; e.g. (add-event! editable ENTER {:callback #(print "ENTER")})
  (swap! editable update-in [:events] #(assoc % event event-definition)))

(defn set-magic! [editable magic-char magic-definition]
  "Add a callback for a pre-defined special character: @something, :emoji..."
  (swap! editable update-in [:magics] #(assoc % magic-char magic-definition)))

(defn- switch-editable [& {:keys [enable disable] :or {enable false disable false}}]
  "unlocking or locking the editable passed as :enable or :disable"
  (when-let [dom-node (:div (deref (or enable disable)))]
    (goog.dom.xml/setAttributes dom-node #js {:contentEditable (not disable)})
    (if disable
      (goog.dom.classlist/add    dom-node "locked")
      (goog.dom.classlist/remove dom-node "locked")))
  (swap! (or enable disable) assoc :locked (not enable)))

(defn- switch-editables [keyhandler mode & [editable]]
  "proxy function for enable and disable, mode is :enable or :disable"
  (if editable
    (doseq [elem (:editables-list @keyhandler)] (if (= editable elem) (switch-editable mode elem)))
    (doseq [elem (:editables-list @keyhandler)] (switch-editable mode elem))))

(defn disable [keyhandler & [editable]]
  "Prevent the editable div from receiving input"
  (switch-editables keyhandler :disable editable))

(defn enable [keyhandler & [editable]]
  "Enable the editable div to receive input"
  (switch-editables keyhandler :enable editable))

(defn- is-platform-special? [event]
  "Return TRUE if we detect Ctrl-TAB, Meta-TAB, Ctrl-C, Meta-C"
  (or (= -1 (.-keyCode event))              ;; ignore keyup events?
      (and (.-platformModifierKey event)
           (or (= (or (and goog.userAgent.MAC (.-META goog.events/KeyCodes))
                      (.-CTRL goog.events/KeyCodes)) (.-keyCode event))
               (= 99 (.-keyCode event))     ;; upper-case C letter
               (= 67 (.-keyCode event)))))) ;; lower-case c letter

(defn- is-at-magic-point? [editable text-to-cursor]
  (some (fn [[magic-char magic-definition]]
          (if (> (count magic-char) 0)
            (let [regex (js/RegExp. (str "(?:^|\\W+)(" magic-char ")$"))
                  match (.exec regex text-to-cursor)]
              (if match {:index            (+ (.-index match) (- (count (first match)) (count (second match))))
                         :magic-char       magic-char
                         :callback         (:callback magic-definition)}))))
        (:magics @editable)))

(defn- mark-range [range magic-char]
  (let [saved-range (goog.editor.range/saveUsingNormalizedCarets range)
        abstract    (.toAbstractRange saved-range)
        content     (.getValidHtml abstract)
        new-node    (goog.dom/createDom "span" #js {:id "marker"})]
    (set! (.-innerHTML new-node) content)
    (.replaceContentsWithNode abstract new-node)
    ;; Really move the caret at the end of the text after the marker wrap
    (let [text-node (goog.testing.dom/findTextNode magic-char new-node)
          offset    (count magic-char)]
      (.select (goog.dom.Range/createFromNodes text-node offset text-node offset)))
    saved-range))

(defn- replace-current-content [editable new-node-description]
  (when-let [saved-range (:saved-range (:active-match @editable))]
    (let [abstract-range (.restore saved-range)
          separator      (:separator new-node-description)
          text-node      (goog.dom/createDom "span" #js {:className "separator"})]
      (if separator
        (set! (.-innerHTML text-node) "&nbsp;"))
      (if-let [new-node (:element new-node-description)]
        (.replaceContentsWithNode abstract-range new-node)
        (.removeContents abstract-range))
      (when-let [container (.getContainerElement abstract-range)]
        (goog.dom/insertSiblingAfter text-node container)
        (goog.dom/flattenElement container))
      (let [new-cursor (goog.dom.Range/createFromNodeContents text-node)]
        (goog.dom/flattenElement text-node)
        (.collapse new-cursor)
        (.select new-cursor))))
  (swap! editable dissoc :active-match))

(defn- maybe-cancel-current-mark [editable callback]
  (when-let [saved-range (:saved-range (:active-match @editable))]
    (when-let [abstract-range (.toAbstractRange saved-range)]
      (if-let [container (.getContainerElement abstract-range)]
        (goog.dom/flattenElement container)))
    (.dispose saved-range))
  (swap! editable dissoc :active-match)
  (if callback (callback)))

(defn get-menu-position [editable event]
  (if-let [editable-div (:div @editable)]
    (if-let [padding-box (goog.style/getPaddingBox editable-div)]
      (when-let [saved-range (:saved-range (:active-match @editable))]
        (let [start-caret        (.getCaret saved-range true)
              position-start-div (goog.style/getClientPosition start-caret)
              viewport           (goog.style/getClientViewportElement start-caret)
              viewport-size      (goog.style/getSize viewport)]
          {:div-left        (.-left padding-box)
           :div-right       (.-right padding-box)
           :event-x         (.-x position-start-div)
           :event-y         (.-y position-start-div)
           :viewport-width  (.-width viewport-size)
           :viewport-height (.-height viewport-size)})))))

(defn- generate-future-text [current-text event]
  "Workaround as the character from the current event is not yet in the DOM"
  (if (goog.events/KeyCodes.isCharacterKey (.-keyCode event))
    (str current-text (js/String.fromCharCode (.-charCode event)))
    (if (and (= (.-keyCode event) (.-BACKSPACE goog.events/KeyCodes)) (> (count current-text) 0))
      (subs current-text 0 (- (count current-text) 1))
      current-text)))

(defn- act-on-key [event editable & {:keys [on-esc-callback]}]
  (if-let [active-match (:active-match @editable)]
    ;; THEN we have an action, callback
    (when-let [match-callback (:callback active-match)]
      (if-let [abstract-range (.toAbstractRange (:saved-range active-match))]
        ;; THEN
        (if-let [text (generate-future-text (.getText abstract-range) event)]
          (if (> (count text) 0)
            (when-let [replacement-node (match-callback text event editable)]
              ;; here the callback is returning an accepted choice to replace in the editable-div
              (replace-current-content editable replacement-node))
            (maybe-cancel-current-mark editable on-esc-callback))
          (maybe-cancel-current-mark editable on-esc-callback))
        ;; ELSE, cancel as we can't extract a range
        (maybe-cancel-current-mark editable on-esc-callback)))
    ;; ELSE try to find an action
    (when-let [browser-range (goog.dom.Range/createFromWindow js/window)]
      (when (.isCollapsed browser-range)
        (let [cursor-offset (.getEndOffset  browser-range)
              anchor-node   (.getAnchorNode browser-range)
              new-range     (goog.dom.Range/createFromNodes anchor-node 0 anchor-node cursor-offset)
              text          (.getText new-range)]
          (if (> (count text) 0)
            (if-let [match-result (is-at-magic-point? editable text)]
              (let [matched-range (goog.dom.Range/createFromNodes anchor-node (:index match-result)
                                                                  anchor-node cursor-offset)
                    saved-range   (mark-range matched-range (:magic-char match-result))]
                (swap! editable assoc :active-match {:saved-range saved-range
                                                     :callback    (:callback match-result)})
                (recur event editable on-esc-callback)))))))))

(defn- act-on-editable-if-matching [event editable]
  ;; the editable needs to be active, not locked, and to match our target
  (when (or (and (:active @editable) (not (:locked @editable))
                 (= (.-target event) (:div @editable))))
    ;; a key was typed in this input area, remove the placeholder
    (when (:placeholder @editable)
      (goog.dom/removeNode (:placeholder @editable))
      (swap! editable dissoc :placeholder))
    ;; send a "is typing..." notification back via the callback if defined
    (let [typing-callback (:typing-callback @editable)]
      (if typing-callback (typing-callback)))
    ;; check whether the size has changed and callback when it did
    (let [current-height (.-height (goog.style/getSize (:div @editable)))]
      (when-not (= current-height (:previous-height @editable))
        (swap! editable assoc :previous-height current-height)
        (let [new-size-callback (:new-size-callback @editable)]
          (if new-size-callback (new-size-callback)))))
    ;; call the specific keys callback if defined
    (let [ESC-event-definition (get (:events @editable) ESC)]
      (condp = (.-keyCode event)
        ENTER (if-not (or (.-shiftKey event) (.-repeat event) (.-ctrlKey event))
                (when-let [event-definition (get (:events @editable) ENTER)]
                  (when-let [on-enter-callback (:callback event-definition)]
                    (.preventDefault event)
                    (on-enter-callback)
                    (if (:act-on-magic event-definition)
                      (act-on-key event editable)))))
        ESC   (when-let [event-definition (get (:events @editable) ESC)]
                (when-let [on-esc-callback (:callback event-definition)]
                  (.preventDefault event)
                  (maybe-cancel-current-mark editable on-esc-callback)))
        (act-on-key event editable :on-esc-callback (:callback ESC-event-definition))))
    ;; return true when we matched the div
    true))

(defn key-on-default [keyhandler keycode]
  (let [default-target (deref (:default-editable @keyhandler))
        new-target-div (:div default-target)]
    (when (and (:active default-target) (not (:locked default-target)))
      (really-focus new-target-div)
      (reset-cursor! keyhandler new-target-div)
      ;; this does not work well on Firefox: the character doesn't get inserted in the div
      (goog.testing.events/fireKeySequence new-target-div keycode))))

(defn- on-key-event [event keyhandler]
  (let [current-target-tag (clojure.string/lower-case (.-tagName (.-target event)))
        editables          (:editables-list @keyhandler)]
    ;; ignore legacy targets, i.e. the search box and platform specific combos (Ctrl-TAB, Ctrl-V)
    (when-not (or (is-platform-special? event) (= current-target-tag "input") (= current-target-tag "textarea"))
      ;; look for one single editable that's active and targetted
      (if-not (some #(act-on-editable-if-matching event %) editables)
        ;; if we didn't find any, then try to move to the default target instead
        (key-on-default keyhandler (.-keyCode event))))))

(defn init [root-node]
  "initialise a keyhandler for the elements contained in the DOM root-node"
  (let [listenable (goog.events.KeyHandler. root-node)
        keyhandler (atom {:editables-list []
                          :listenable     listenable})]
    (goog.events/listen listenable (.-KEY goog.events.KeyHandler/EventType) #(on-key-event % keyhandler))
    (goog.events/listen root-node (.-BLUR goog.events/EventType) #())
    keyhandler))
