(ns projecteuler.problem16)

(reduce
  #(+ % (read-string (str %2))) 0 (str (.pow (BigInteger. "2") 1000)))