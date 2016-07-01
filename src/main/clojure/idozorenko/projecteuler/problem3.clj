(ns idozorenko.projecteuler.problem3)

(defn get-divisors [num]
  (flatten
    (for [x (range 1 (inc (Math/sqrt num)))
          :when (zero? (rem num x))]
      (let [y (/ num x)]
        (if (not= x y)
          [x y]
          x)))))

(->> (get-divisors 600851475143)
  (map get-divisors)
  (filter #(= 2 (count %)))
  (map second)
  (apply max))