(ns projecteuler.problem15)

(defn binom [n k]
  (reduce *
    (for [i (range 1 (inc k))]
     (/ (- (+ n 1) i) i))))
