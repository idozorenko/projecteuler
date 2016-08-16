(ns idozorenko.projecteuler.problem91)

(defn solve []
  (count
    (let [size (inc 50)
          [x3 y3] [0 0]]
      (filter
        (fn [[[x1 y1] [x2 y2]]]
          (let [[a b c] (sort
                          [(+ (* (- y3 y2) (- y3 y2)) (* (- x3 x2) (- x3 x2)))
                           (+ (* (- y2 y1) (- y2 y1)) (* (- x2 x1) (- x2 x1)))
                           (+ (* (- y3 y1) (- y3 y1)) (* (- x3 x1) (- x3 x1)))])]
            (and (not (zero? (* a b c))) (= (+ a b) c))))
        (for [x1 (range size)
              y1 (range size)
              x2 (range size)
              y2 (range size)
              :when (> (+ (* size x1) y1) (+ (* size x2) y2))]
          [[x1 y1] [x2 y2]])))))