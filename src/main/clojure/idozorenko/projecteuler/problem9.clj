(ns idozorenko.projecteuler.problem9)

(apply *
  (first
    (filter (fn [[a b c]] (and (= (* c c) (+ (* a a) (* b b)))))
      (for [a (range 1 1000)
            b (range a (- 1000 a))
            :let [c (- 1000 a b)]]
        [a b c]))))