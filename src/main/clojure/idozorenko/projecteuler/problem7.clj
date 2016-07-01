(ns idozorenko.projecteuler.problem7)

(first
  (drop 1E4
    (filter #(.isProbablePrime (BigInteger. (str %)) 7)
      (iterate inc 1))))