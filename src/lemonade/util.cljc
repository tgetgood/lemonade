(ns lemonade.util
  (:require [net.cgrand.macrovich :as macros :include-macros true]))

(def lemonade-types
  '[Region Composite Frame
    Line Bezier Arc
    Circle Annulus PolyLine Rectangle
    AffineTransformation
    RawText Text])

(defn import-lemonade-types
  "Imports core Lemonade shape types into the current ns appropriately for either
  clj or cljs. Avoids messy and brittle ns forms."
  ;; REVIEW: This is not idiomatic, nor is it recommended. What are the
  ;; downsides?
  []
  #?(:clj `(import [lemonade.core ~@lemonade-types])
     :cljs `(require [lemonade.core :refer ~lemonade-types])))

(macros/deftime
  (defmacro implement-sequentials
    "Generates boilerplate to implement a protocol identically for all
     sequential things."
    {:style/indent [1 :form [1]]}
    [prot & methods]
    (let [types (macros/case :cljs '[List
                                     LazySeq
                                     PersistentVector
                                     IndexedSeq
                                     ChunkedSeq
                                     ArrayList]
                             :clj '[clojure.lang.PersistentVector
                                    clojure.lang.PersistentList
                                    clojure.lang.ArraySeq
                                    clojure.lang.IndexedSeq
                                    clojure.lang.PersistentVector$ChunkedSeq
                                    clojure.lang.LazySeq])]
      `(extend-protocol ~prot
         ~@(mapcat (fn [a b] `[~a ~@b]) types (repeat methods))))))
