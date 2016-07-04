(ns idozorenko.projecteuler.problem57)

(loop [i 999
       f 2
       cnt 0]
  (if (zero? i)
    cnt
    (let [cnt (cond-> cnt
                (let [fs (+ 1 (/ 1 f))]
                  (>
                    (count (str (numerator fs)))
                    (count (str (denominator fs))))) inc)]
      (recur (dec i) (+ 2 (/ 1 f)) cnt))))