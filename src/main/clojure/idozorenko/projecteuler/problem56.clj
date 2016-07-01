(ns idozorenko.projecteuler.problem56)

(reduce max
  (for [i (range 0 100)
        j (range 0 100)]
    (reduce #(+ (- (int %2) 48) %) 0
      (str (.pow (BigInteger. (str i)) j)))))