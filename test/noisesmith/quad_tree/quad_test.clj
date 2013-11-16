(ns noisesmith.quad-tree.quad-test
  (:require [clojure.test :as test :refer [deftest testing is]]
            [noisesmith.quad-tree.ops :as ops]
            [noisesmith.quad-tree.quad :as quad]))

(deftest make-quad
  (is (= (into {} (quad/make-quad 0 0 1 1))
         {:ne nil
          :se nil
          :sw nil
          :nw nil
          :x 0
          :y 0
          :x' 1
          :y' 1
          :pr {}})))

(deftest split
  (is (= {:x 0
          :y 0
          :x' 1/2
          :y' 1/2
          :x'' 1
          :y'' 1}
         (quad/split (quad/make-quad 0 0 1 1)))))

(deftest impose
  (let [parent (quad/make-quad 0 0 2 2)]
    (is (= (quad/make-quad 0 0 1 1)
           (quad/impose parent :nw)))
    (is (= (quad/make-quad 1 0 2 1)
           (quad/impose parent :ne)))
    (is (= (quad/make-quad 1 1 2 2)
           (quad/impose parent :se)))
    (is (= (quad/make-quad 0 1 1 2)
           (quad/impose parent :sw)))))

(deftest unfold
  (let [parent (quad/make-quad 0 0 1024 1024)
        mark (fn [e]
               (update-in e [:pr] assoc :mark :X))
        deeper? (fn [p e]
                  (and (< (:x e) 512)
                       (< (:y e) 512)
                       (> (ops/area e) 8192)))
        parent (quad/unfold parent mark deeper?)
        all (set (quad/walk #(select-keys % [:x :y :x' :y']) parent))]
    (is (= #{[0 0 1024 1024]
             [0 0 512 512]
             [256 384 384 512]
             [256 128 384 256]
             [256 256 384 384]
             [256 256 512 512]
             [384 0 512 128]
             [0 0 256 256]
             [0 256 256 512]
             [0 0 128 128]
             [0 128 128 256]
             [128 0 256 128]
             [128 256 256 384]
             [128 384 256 512]
             [256 0 384 128]
             [256 0 512 256]
             [384 128 512 256]
             [0 256 128 384]
             [384 256 512 384]
             [0 384 128 512]
             [384 384 512 512]
             [128 128 256 256]}
           (set (map (juxt :x :y :x' :y') all))))))
