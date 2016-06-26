(ns idozorenko.projecteuler.problem21)

(defn divisors [n]
  (flatten
    (for [x (range 1 (inc (int (Math/sqrt n))))
         :when (and (not= x n) (zero? (rem n x)))]
     (let [d (/ n x)]
       (if (and (not= d n) (not= d x))
         [x d]
         [x])))))

(defn amicable-numbers []
  (let [nums (range 1 10000)
        divs (into {} (map (fn [num] [num (reduce + (divisors num))]) nums))

        amicable (for [x nums
                       :when (let [d1 (divs x)
                                   d2 (divs d1)]
                               (and (not= x d1) (= d2 x)))]
                   x)]
    amicable))

#_(reduce + (amicable-numbers))