(ns noisesmith.quad-tree.geometry-test
  (:require [clojure.test :as test :refer [deftest testing is]]
            [noisesmith.quad-tree.geometry :as geom]))

(deftest non-overlap?
  (testing "west"
      (is (= true
             (geom/non-overlap? {:x 0 :y 0 :x' 1 :y' 1}
                                {:x 0 :y 0 :x' 0 :y' 0}))
          (= true
             (geom/non-overlap? {:x 0 :y 0 :x' 1 :y' 1}
                                {:x -1 :y -1 :x' 0 :y' 0})))))

(deftest fully-overlapped?
  (testing "equal is overlap"
    (is (= true
           (geom/fully-overlapped? {:x 0 :y 0 :x' 1 :y' 1}
                                   {:x 0 :y 0 :x' 1 :y' 1})))))
