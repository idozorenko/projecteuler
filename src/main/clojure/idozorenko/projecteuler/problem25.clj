(ns idozorenko.projecteuler.problem25)

(defn fib [pred]
  (loop [a (bigint 1)
         b (bigint 1)
         i 1]
    (if (pred b)
      (inc i)
      (let [s (+ a b)]
       (recur b s (inc i))))))

(defn first-fib-longer-than [n]
  (fib #(= (count (str %)) n)))