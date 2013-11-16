(ns noisesmith.quad-tree.ops)

(defn bit-put
  [n value position]
  (if (= value 0)
    (bit-clear n position)
    (bit-set n position)))

(defn eps=
  [& [epsilon]]
  (let [epsilon (or epsilon 0.5)]
    (fn [a b]
      (let [diff (- a b)]
        (<= (if (< diff 0)
              (* diff -1)
              diff)
            epsilon)))))

(defn eps<
  [& [epsilon]]
  (let [epsilon (or epsilon 0.5)]
    (fn [& args]
      (every? #(< % epsilon)
              (apply map - (partition 2 1 args))))))

(defn eps>
  [& [epsilon]]
  (let [epsilon (or epsilon 0.5)]
    (fn [& args]
      (every? #(< % epsilon)
              (apply map #(- %2 %1) (partition 2 1 args))))))

(defn show-bits
  [n c]
  (doall
   (take c
         (iterate
          (fn [n]
            (if (bit-test n 0)
              (print \1)
              (print \0))
            (bit-shift-right n 1))
          n)))
  (println))

(defn rev
  "reverses the first resolution bits of value"
  [value resolution]
  (reduce (fn [flipping i]
            (if (bit-test value i)
              (bit-set flipping (- (dec resolution) i))
              flipping))
          0
          (range resolution)))

(defn area
  "the area of a quad"
  [q]
  (* (- (:x' q) (:x q))
     (- (:y' q) (:y q))))
