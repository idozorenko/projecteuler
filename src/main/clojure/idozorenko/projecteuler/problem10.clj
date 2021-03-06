(ns projecteuler.problem10)

(reduce +
  (take-while #(< % 2E6)
    (filter #(.isProbablePrime (BigInteger. (str %)) 7)
      (iterate inc 1))))