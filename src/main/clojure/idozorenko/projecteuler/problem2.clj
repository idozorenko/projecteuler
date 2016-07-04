(ns idozorenko.projecteuler.problem2)

(loop [a 1
       b 1
       sum 0]
  (if (> b 4E6)
    sum
    (let [a (+ a b)]
      (recur b a (if (odd? a) sum (+ a sum))))))