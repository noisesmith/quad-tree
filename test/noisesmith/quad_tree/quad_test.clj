(ns noisesmith.quad-tree.quad-test
  (:require [clojure.test :as test :refer [deftest testing is]]
            [noisesmith.quad-tree.quad :as quad]))

(deftest make-quad
  (is (= (into {} (quad/make-quad [0 0] [1 1]))
         {:ne nil
          :se nil
          :sw nil
          :nw nil
          :x 0
          :y 0
          :x' 1
          :y' 1
          :properties #{}})))
