(ns idozorenko.projecteuler.problem33)

(reduce *
  (for [i (range 10 100)
        j (range i 100)
        :let [is1 (mod i 10)
              js1 (int (/ j 10))
              is2 (int (/ i 10))
              js2 (mod j 10)]
        :when (or
                (and (= is1 js1) (not (zero? js2)) (= (/ i j) (/ is2 js2)))
                (and (= is2 js2) (not (zero? js1)) (= (/ i j) (/ is1 js1))))]
    (/ i j)))