(ns noisesmith.quad-tree.quad
  (:require [noisesmith.quad-tree.ops :as ops]))

(defrecord quad [ne se sw nw x x' y y' properties])

(defn make-quad
  [[x y] [x' y']]
  (->quad nil nil nil nil x x' y y' #{}))

(defn split
  [{:keys [x x' y y'] :as q}]
  {:x x
   :y y
   :x' (+ x (/ (- x' x) 2))
   :y' (+ y (/ (- y' y) 2))
   :x'' x'
   :y'' y'})

(defn path
  [quadress]
  (let [res (:res quadress)
        path-x (ops/rev (:bits-x quadress) res)
        path-y (ops/rev (:bits-y quadress) res)]
    (for [step (range res)]
      (case [(bit-test path-x step) (bit-test path-y step)]
        ([true true]) :se
        ([true false]) :ne
        ([false true]) :sw
        ([false false]) :nw))))

(defn impose
  [quad direction]
  (let [{:keys [x y x' y' x'' y'']} (split quad)]
    (case direction
      :nw (or (:nw quad)
              (make-quad [x y] [x' y']))
      :ne (or (:ne quad)
              (make-quad [x' y] [y y']))
      :se (or (:se quad)
              (make-quad [x' y'] [x'' y'']))
      :sw (or (:sw quad)
              (make-quad [x y'] [x' y''])))))

(defn unfold
  "From some seed, we grow a tree." 
  [quad updater deeper?]
  (let [quad (updater quad)
        nw (impose quad :nw)
        quad (if (deeper? nw)
               (assoc quad :nw (unfold nw updater deeper?))
               quad)
        ne (impose quad :ne)
        quad (if (deeper? ne)
               (assoc quad :ne (unfold ne updater deeper?))
               quad)
        se (impose quad :se)
        quad (if (deeper? se)
               (assoc quad :se (unfold ne updater deeper?))
               quad)
        sw (impose quad :sw)
        quad (if (deeper? sw)
               (assoc quad :sw (unfold sw updater deeper?))
               quad)]
    quad))

(defn non-overlap?
  [{:keys [x y x' y'] :as quad} [x'' y''] [x''' y''']]
  (or (< x' x'')
      (< y' y'')
      (> x x''')
      (> y y''')))

(def overlap?
  (comp not non-overlap?))

(defn fully-overlapped?
  [{:keys [x y x' y'] :as quad} [x'' y''] [x''' y''']]
  (and (<= x'' x x' x''')
       (<= y'' y y' y''')))

(defn insert
  [[x y :as upper-left] [x' y' :as lower-right] quad mark]
  (let [deeper? (fn [q]
                  (println [x y x' y'] ((juxt :x :y :x' :y') q))
                  (or (= [x y x' y'] ((juxt :x :y :x' :y') q))
                      (and (overlap? q upper-left lower-right)
                           (not (fully-overlapped? q upper-left lower-right)))))
        updater (fn [q]
                  (if (fully-overlapped? q upper-left lower-right)
                    (mark q)
                    q))]
    (unfold quad updater deeper?)))
