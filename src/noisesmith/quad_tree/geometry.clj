(ns noisesmith.quad-tree.geometry)

(defn west?
  [{x'-a :x'}
   {x-b :x }]
  (<= x'-a x-b))

(defn east?
  [a b]
  (west? b a))

(defn north?
  [{y'-a :y'}
   {y-b :y}]
  (<= y'-a y-b))

(defn south?
  [a b]
  (north? b a))

(defn ylppa
  [args f]
  (apply f args))

(defn non-overlap?
  [a b]
  (first (filter identity
                 (map (partial ylppa [a b])
                      [west? east? north? south?]))))

(def overlap?
  (comp not non-overlap?))

(defn fully-overlapped?
  [{x-a :x y-a :y x'-a :x'  y'-a :y' :as a}
   {x-b :x y-b :y x'-b  :x'  y'-b :y' :as b}]
  (and (<= x-b x-a x'-a x'-b)
       (<= y-b y-a y'-a y'-b)))
