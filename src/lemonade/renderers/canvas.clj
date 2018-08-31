(ns lemonade.renderers.canvas)

(defmacro call [f & args]
  `(Call. ~f ~@(take 6 (concat args (repeat nil)))))

(defn switch [c ctx cmd [cname nargs]]
  [(list 'identical? c cname)
   (if (int? nargs)
     (apply list (symbol (str "." cname)) ctx
            (map (fn [i] (list (symbol (str ".-arg" i)) cmd)) (range 1 (inc nargs))))
     nargs)])

(defmacro unsafe-invoke
  {:style/indent 2
   :doc "Builds an interpreter for canvas instructions. Paths is a list of
   command names and the number of args they expect. All very unsafe. All very
   fast. This macro is intended to make it look a bit less scary."}
  [ctx cmd & paths]
  (let [c (gensym)]
    `(let [~c (.-f ~cmd)]
       (cond
         ~@(mapcat (fn [path] (switch c ctx cmd path)) (partition 2 paths))
         :else (throw (js/Error. (str "Invalid canvas command: " ~cmd)))))))

(defn maybe-reduce [xf acc coll]
  (when coll
    (if (vector? coll)
      (map (fn [x] `(~xf ~acc ~x)) coll)
      (list `(reduce ~xf ~acc ~coll)))))

(defn compile-leaf-inner [pre draw post]
  (let [xf (gensym "xf")
        acc (gensym "acc")]
    `(fn [~xf coll#]
       (as-> coll# ~acc
         ~@(concat (maybe-reduce xf acc pre)
                   (maybe-reduce xf acc draw)
                   (maybe-reduce xf acc post))))))

(defn compile-node-inner [pre recur-on post]
  (let [xf (gensym "xf")
        acc (gensym "acc")]
    `(fn [~xf coll#]
       (as-> coll# ~acc
         ~@(concat (maybe-reduce xf acc pre)
                   [`(reduce ~xf ~acc
                             (lemonade.renderers.canvas/compile ~recur-on))]
                   (maybe-reduce xf acc post))))))

(defn compile-style [style inner]
  `(let [gen# (gensym)
         setters# (simple-style ~style gen#)]
     (if (seq setters#)
       (fn [xf# coll#]
         (as-> coll# acc#
           (xf# acc# *save)
           (reduce xf# acc# setters#)
           (~inner xf# acc#)
           (xf# acc# *restore)
           (xf# acc# (UnSetter. gen#))))
       ~inner)))

(defmacro compile-leaf [{:keys [style pre post draw]}]
  `(persistent! (~(if style
                    (compile-style style (compile-leaf-inner pre draw post))
                    (compile-leaf-inner pre draw post))
                 conj! (transient []))))

(defmacro compile-node [{:keys [style pre recur-on post]}]
  `(persistent! (~(if style
                    (compile-style style (compile-node-inner pre recur-on post))
                    (compile-node-inner pre recur-on post))
                 conj! (transient []))))
