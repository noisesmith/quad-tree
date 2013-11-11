(ns noisesmith.quad-tree.quadress
  (:require [noisesmith.quad-tree.ops :as ops]))

(defrecord quadress [res bits-x bits-y])

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

(declare step)
(defn make
  [x y width height & [epsilon]]
  (let [close (ops/eps= (or epsilon 1))]
    (if (or (> x width)
            (< x 0)
            (> y height)
            (< y 0))
      nil
      (apply ->quadress
             (loop [[pos-x pos-y bits-x bits-y division-x   division-y  res
                     :as input]
                    [0     0     0      0      (/ width 2) (/ height 2) 0]]
               #_ (println input)
               (if (and (close pos-x x)
                        (close pos-y y))
                 [res bits-x bits-y]
                 (recur
                  (step x y
                        pos-x pos-y
                        bits-x bits-y
                        division-x division-y res))))))))

(defn step
  [x y pos-x pos-y bits-x bits-y division-x division-y res]
  #_
  (println (= x pos-x) (= y pos-y)
           x y pos-x pos-y bits division-x division-y res)
  #_
  (Thread/sleep 1000)
  (let [x' (if (< x (+ pos-x division-x)) 0 1)
        pos-x' (+ pos-x (* division-x x'))
        y' (if (< y (+ pos-y division-y)) 0 1)
        pos-y' (+ pos-y (* division-y y'))
        bits-x' (-> bits-x
                    (bit-shift-left 1)
                    (+ x'))
        bits-y' (-> bits-y
                    (bit-shift-left 1)
                    (+ y'))]
    [pos-x'
     pos-y'
     bits-x'
     bits-y'
     (/ division-x 2)
     (/ division-y 2)
     (inc res)]))

(defn get-point
  [quadress resolution]
  (map (partial * (/ resolution (bit-shift-left 1 (:res quadress))))
       ((juxt :bits-x :bits-y) quadress)))

(defn qd-print
  [quadress resolution]
  (let [[point-x point-y] (get-point quadress resolution)
        eps= (ops/eps= 0.5)]
    (doseq [y (range resolution) x (range resolution)]
      (when (zero? x) (println))
      (print
       (if (and (eps= x point-x)
                (eps= y point-y))
         \*
         \_)))))
