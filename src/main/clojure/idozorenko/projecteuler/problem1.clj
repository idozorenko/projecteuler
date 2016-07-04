(ns idozorenko.projecteuler.problem1)

(reduce +
  (for [i (range 1 1000)
        :when (or (zero? (mod i 3)) (zero? (mod i 5)))] i))