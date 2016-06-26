(ns idozorenko.projecteuler.problem29)

(count
  (set
    (for [a (range 2 101)
          b (range 2 101)]
      (reduce * (repeat b (bigint a))))))