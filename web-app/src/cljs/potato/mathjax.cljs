(ns potato.mathjax
  (:require [goog.dom]))

(defn load-mathjax []
  (let [script-tag
        (goog.dom/createDom "script"
                            #js {:type "text/javascript"
                                 :src  "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"})]
    (goog.dom/append (first (goog.dom/getElementsByTagNameAndClass "head")) script-tag)))

(defn add-node [node]
  (if (.-mathjaxLoaded js/window)
    (if (.hasOwnProperty js/window "MathJax")
      (.Queue (.-Hub js/MathJax) #js ["Typeset" (.-Hub js/MathJax) node]))
    (do
      (load-mathjax)
      (set! (.-mathjaxLoaded js/window) true))))
