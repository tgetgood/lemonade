(ns lemonade.hosts
  #?(:cljs (:require [lemonade.hosts.browser-canvas :as browser-canvas]
                     lemonade.core)
     :clj (:require [lemonade.hosts.jvm-quil :as jvm-quil])))

(defrecord Host [opts]
  lemonade.core/Host
  (width [_] ((:width opts)))
  (height [_] ((:height opts)))
  (base [_] (:elem opts))
  (render-fn [_] (:render-fn opts)))

#?(:cljs
   (defn html-canvas
     "Runs lemonade in a the given <canvas> element in the browser."
     [opts]
     (let [b (browser-canvas/host opts)]
       (Host. b))))

#?(:clj
   (defn quil
     "Creates a Processing applet and prepares to run Lemonade in it."
     [opts]
     (Host. (jvm-quil/host opts))))

(defn default-host [opts]
  (#?(:clj quil :cljs html-canvas) opts))
