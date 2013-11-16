(ns noisesmith.quad-tree.quad
  (:require [noisesmith.quad-tree.ops :as ops]
            [noisesmith.quad-tree.geometry :as geom]
            [analemma.svg :as svg]
            [analemma.xml :as xml]))

(defrecord quad [ne se sw nw x x' y y' pr])

(defn make-quad
  [x y x' y']
  (->quad nil nil nil nil x x' y y' {}))

(defn split
  [{:keys [x x' y y'] :as q}]
  {:x x
   :y y
   :x' (+ x (/ (- x' x) 2))
   :y' (+ y (/ (- y' y) 2))
   :x'' x'
   :y'' y'})

(defn impose
  [quad direction]
  (let [{:keys [x y x' y' x'' y'']} (split quad)]
    (case direction
      :nw (or (:nw quad)
              (make-quad x y x' y'))
      :ne (or (:ne quad)
              (make-quad x' y x'' y'))
      :se (or (:se quad)
              (make-quad x' y' x'' y''))
      :sw (or (:sw quad)
              (make-quad x y' x' y'')))))

(defn unfold
  "From some seed, we grow a tree." 
  [quad updater deeper-pred]
  (let [deeper? (partial deeper-pred quad)
        quad (updater quad)
        nw (impose quad :nw)
        quad (if (deeper? nw)
               (assoc quad :nw (unfold nw updater deeper-pred))
               quad)
        ne (impose quad :ne)
        quad (if (deeper? ne)
               (assoc quad :ne (unfold ne updater deeper-pred))
               quad)
        se (impose quad :se)
        quad (if (deeper? se)
               (assoc quad :se (unfold se updater deeper-pred))
               quad)
        sw (impose quad :sw)
        quad (if (deeper? sw)
               (assoc quad :sw (unfold sw updater deeper-pred))
               quad)]
    quad))

(defn insert
  [quad mark place]
  (let [deeper? (fn [p q]
                  (and (geom/overlap? q place)
                       (not (geom/fully-overlapped? p place))))
        updater (fn [q]
                  (if (geom/fully-overlapped? q place)
                    (mark q)
                    q))]
    (unfold quad updater deeper?)))

(defn ->svg
  [{:keys [x y x' y'] :as quad}]
  (svg/rect x y (- x' x) (- y' y)))

(defn ->color
  [quad {:keys [fg bg opacity] :as color
         :or {fg "#000000"
              bg "#000000"
              opacity 0.1}}]
  (svg/style quad
             :fill-opacity opacity
             :stroke fg
             :fill bg))

(defn walk
  "Do something for each quad within the tree"
  [f quad]
  (cons
   (f quad)
   (mapcat (partial walk f)
           (filter identity [(:nw quad)
                             (:ne quad)
                             (:se quad)
                             (:sw quad)]))))

(defn render
  [quad-tree & [file colors]]
  (let [file (or file "test.svg")
        colors (or colors (cycle [{}]))
        quads (walk identity quad-tree)]
    (->> (map #(->color (->svg %2) (or (:color (:pr %2)) %))
              colors quads)
         (apply svg/svg)
         xml/emit
         (spit file))))
