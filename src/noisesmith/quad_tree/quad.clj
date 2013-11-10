(ns noisesmith.quad-tree.quad)

(defrecord quad [ne se sw nw x x' y y'])

(defn make-quad
  [[x y] [x' y']]
  (->quad nil nil nil nil x x' y y'))

(defn split
  [{:keys [x x' y y'] :as q}]
  (let [x'' (+ x (/ (- x' x) 2))
        y'' (+ y (/ (- y' y) 2))]
    (assoc q
      :nw (make-quad [x y] [x'' y''])
      :ne (make-quad [x'' y] [x' y''])
      :se (make-quad [x'' y''] [x' y'])
      :sw (make-quad [x y''] [x'' y']))))

#_
(defn point
  [q [x y]]
  (cond 
   (and (= x (:x q))
        (= y (:y q))) q
   (< x (:x q)) [:x -1]
   (> x (:x' q)) [:x 1]
   (< y (:y q)) [:y -1]
   (> y (:y' q)) [:y 1]))

#_
(defn mark
  [q [x y] [x' y']]
  (cond (and (= (:x q) x)
             (= (:y q) y)
             (= (:x' q) x')
             (= (:y' q) y'))
        (make-quad [x y] [x' y'])
        ()))
              
              
(defrecord quadress [res bits])


(declare quadress-step)
(defn make-quadress
  [x y width height & [epsilon]]
  (let [resolution (or epsilon 1)]
    (if (or (> x width)
            (< x 0)
            (> y height)
            (< y 0))
      nil
      (apply ->quadress
             (loop [[pos-x pos-y bits division-x  division-y  res]
                    [0     0     0    (/ width 2) (/ height 2) 0 1]]
               (if (and (< (Math/abs (- pos-x x)) epsilon)
                        (< (Math/abs (- pos-y y)) epsilon))
                 [res bits]
                 (recur
                  (quadress-step x y
                                 pos-x pos-y
                                 bits
                                 division-x division-y res))))))))

(defn bit-put
  [n value position]
  (if (= value 0)
    (bit-clear n position)
    (bit-set n position)))

(defn quadress-step
  [x y pos-x pos-y bits division-x division-y res]
  #_
  (println (= x pos-x) (= y pos-y)
           x y pos-x pos-y bits division-x division-y res)
  #_
  (Thread/sleep 1000)
  (let [x' (if (< x (+ pos-x division-x)) 0 1)
        pos-x' (+ pos-x (* division-x x'))
        y' (if (< y (+ pos-y division-y)) 0 1)
        pos-y' (+ pos-y (* division-y y'))
        bits' (-> bits
                  (bit-shift-left 2)
                  (bit-put x' 0)
                  (bit-put y' 1))]
    [pos-x'
     pos-y'
     bits'
     (/ division-x 2)
     (/ division-y 2)
     (inc res)]))

(defn quadress-area
  [x x' y y'])
