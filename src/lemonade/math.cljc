(ns lemonade.math)

;;;;; Trig

(def π
  #?(:cljs js/Math.PI
     :clj Math/PI))

(def pi
  "Ratio of circumference to diameter of a circle.
  For those who don't like programming with unicode."
  π)

(def e
  #?(:clj Math/E
     :cljs js/Math.E))

(defn deg->rad
  "Converts degrees to radians."
  [d]
  (* π (/ d 180)))

(defn rad->deg
  "Convert radians into degrees."
  [r]
  (/ (* r 180) π))

(defn sin [x]
  (#?(:cljs js/Math.sin :clj Math/sin) x))

(defn cos [x]
  (#?(:cljs js/Math.cos :clj Math/cos) x))

(defn atan [x]
  (#?(:clj Math/atan :cljs js/Math.atan) x))

(defn abs [x]
  (if (< x 0)
    (- x)
    x))

(defn floor [x]
  (#?(:clj Math/floor :cljs js/Math.floor) x))

(defn sqrt [x]
  (#?(:cljs js/Math.sqrt :clj Math/sqrt) x))

(defn nan? [x]
  (#?(:cljs js/isNaN :clj Double/isNaN) x))

(defn log
  ([x]
   (#?(:clj Math/log :cljs js/Math.log) x))
  ([base x]
   (/ (log x) (log base))))

(defn exp
  "Exponential function. Returns b^n. If b not specified defaults to Euler's
  number."
  ([n]
   (#?(:clj Math/exp :cljs js/Math.exp) n))
  ([b n]
   (#?(:clj Math/pow :cljs js/Math.pow) b n)))

(defn dot [x y]
  (reduce + (map * x y)))

(defn norm [[x y]]
  (sqrt (+ (* x x) (* y y))))

(defn unit [{[x1 y1 :as from] :from [x2 y2 :as to] :to :as line}]
  (let [l (norm (mapv - to from))]
    [(/ (- x2 x1) l) (/ (- y2 y1) l)]))

(defn dist [[x1 y1] [x2 y2]]
  (norm [(- x1 x2) (- y1 y2)]))

;;;;; Linear and Affine

(def ^:private **
  "Temporary workaround for spec and big arithmetic"
  #?(:clj *' :cljs *))

(def idm
  "The 2x2 identity matrix"
  [1.0 0.0 0.0 1.0])

(defn det
  "Returns the determinant of a 2x2 matrix"
  [a b c d]
  (- (** a d) (** b c)))

(defn atx
  "Convenience fn for building atx maps"
  ([m]
   (atx m [0 0]))
  ([m b]
   {:matrix      m
    :translation b}))

(def id (atx idm [0.0 0.0]))

(defn invert-atx
  "Returns matrix corresponding to the inverse affine transform."
  [{[a b c d] :matrix [x y] :translation}]
  (let [abs (det a b c d)
        [a' b' c' d'] (map #(/ % abs) [d (- b) (- c) a])
        x' (- (+ (** a' x) (** b' y)))
        y' (- (+ (** c' x) (** d' y)))]
    (atx [a' b' c' d'] [x' y'])))

(defn comp-atx
  "Returns the composition of affine transformations"
  ([] id)
  ([a] a)
  ([{[a b c d] :matrix [x y] :translation}
    {[a' b' c' d'] :matrix [x' y'] :translation}]
   (atx [(+ (* a a') (* b c'))
         (+ (* a b') (* b d'))
         (+ (* c a') (* d c'))
         (+ (* c b') (* d d'))]
        [(+ x (* a x') (* b y'))
         (+ y (* c x') (* d y'))]))
  ([a b & more] (reduce comp-atx (comp-atx a b) more)))

(defn apply-atx
  "Applies affine tx to a point and returns the result."
  [{[a b c d] :matrix [e f] :translation} [x y]]
  [(+ (* a x) (* b y) e) (+ (* c x) (* d y) f)])
