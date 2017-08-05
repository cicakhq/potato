(ns potato.component
  (:require        [cljsjs/preact :as p])
  (:require-macros [potato.component :refer [preact-method]]))

(defn find-dom-node [component]
  (or (and component (.-base component) component)))

(def ^:dynamic *component*
  "Within a component lifecycle function, is be bound to the underlying PreactJS instance." nil)

(def ^:private lifecycle-impls
  "Mapping of public lifecycle API to internal PreactJS API."
  (let [basic (fn [impl]
                (preact-method []
                  (apply impl
                    (find-dom-node *component*)
                    (.-value (.-props *component*))
                    (.-constants (.-props *component*)))))
        with-old-value (fn [impl]
                         (preact-method [prev-props _]
                           (apply impl
                             (find-dom-node *component*)
                             (.-value (.-props *component*))
                             (.-value prev-props)
                             (.-constants (.-props *component*)))))
        with-nil-old-value (fn [impl]
                             (preact-method []
                               (apply impl
                                 (find-dom-node *component*)
                                 (.-value (.-props *component*))
                                 nil
                                 (.-constants (.-props *component*)))))
        with-callback (fn [impl]
                        (preact-method [cb]
                          (apply impl
                            (find-dom-node *component*)
                            cb
                            (.-value (.-props *component*))
                            (.-constants (.-props *component*)))))]
    {:on-mount {:componentDidMount basic}
     :on-update {:componentDidUpdate with-old-value}
     :on-unmount {:componentWillUnmount basic}
     :on-render {:componentDidUpdate with-old-value
                 :componentDidMount with-nil-old-value}}))

(defn- build-lifecycle-impls
  [opts-map]
  (reduce (partial merge-with
            (fn [_ _]
              (throw "Component definition should not provide handlers for both :on-render and (:on-mount | :on-update).")))
    (map (fn [[key impl]]
           (when-let [impl-map (lifecycle-impls key)]
             (into {} (for [[method impl-ctor] impl-map]
                        [method (impl-ctor impl)]))))
      opts-map)))

(defn component
  "Return a factory function that will create a PreactElement, using the provided function as the
  'render' method for a PreactJS component, which is created and instantiated behind-the-scenes.
  The given render function should take a single immutable value as its first argument, and return
  a single PreactElement. Additional arguments to the returned factory function are
  /constant arguments/  which will be passed on as additional arguments to the  supplied render
  function, but will *not* be included in any calculations regarding whether the element should
  re-render. As such, they are suitable for values that will remain constant for  the lifetime of
  the rendered element, such as message channels and configuration objects.
  The optional 'opts' argument is a map which contains additional configuration keys:
     :keyfn - a single-argument function which is invoked at component construction time. It is
     passed the component's value, and returns the PreactJS key used to uniquely identify this
     component among its children.
     :name - the name of the element, used for debugging purposes.
     :on-mount - A function which will be invoked once, immediately after initial rendering occurs.
     It is passed the DOM node, the value and any constant args passed to the render fn. This maps
     to the 'componentDidMount' lifecycle method in PreactJS.
     :on-update - A function which will be invoked immediately after an update is flushed to the DOM,
     but not on the initial render. It is is passed the underlying DOM node, the value,
     the _old_ value, and any constant args passed to the render fn. This maps to the
     'componentDidUpdate' lifecycle method in PreactJS.
     :on-unmount - A function which will be invoked immediately before a the component is unmounted
     from the DOM. It is passed the underlying DOM node, the most recent value and the most recent
     constant args passed to the render fn. This maps to the 'componentWillUnmmount' lifecycle
     method in PreactJS.
     :on-render - A function which will be invoked immediately after the DOM is updated, both on the
     initial render and any subsequent updates. It is is passed the underlying DOM node, the
     value, the _old_  value (which will be nil in the case of the initial render) and any constant
     args passed to the render fn. This maps to both the 'componentDidMount' and
     'componentDidUpdate' lifecycle methods in PreactJS.
  The *component* dynamic var will be bound to the underlying PreactJS component for all invocations
  of the render function and invocations of functions defined in the opts map."
  ([renderer] (component renderer {}))
  ([renderer opts]
    (let [impl (merge
                 (when (:name opts) {:displayName (:name opts)})
                 {:shouldComponentUpdate (preact-method [next-props _]
                                           (not= (.-value (.-props *component*))
                                                 (.-value next-props)))
                  :render (preact-method []
                            (apply renderer
                              (.-value (.-props *component*))
                              (.-constants (.-props *component*))))}
                 (build-lifecycle-impls opts))
          preact-component (p/Component (clj->js impl))] ;;; is this correct?
      (fn [value & constant-args]
        (let [props (js-obj)]
          (set! (.-value props) value)
          (set! (.-constants props) constant-args)
          (when-let [keyfn (:keyfn opts)]
            (set! (.-key props) (keyfn value)))
          (p/h preact-component props))))))

(defn unmount
  "Remove a mounted Element from the given DOM node."
  [node]
  (p/render (p/h nil nil) node))
