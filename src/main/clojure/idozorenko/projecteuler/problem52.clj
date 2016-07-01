(ns idozorenko.projecteuler.problem52)

(loop [i 123456]
  (if (apply = (for [j (range 2 7)]
                 (group-by identity (str (* j i))))) i
    (recur (inc i))))