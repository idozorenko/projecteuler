(ns projecteuler.problem12)

(defn triangle-seq
  ([] (triangle-seq 1 0))
  ([n sum] (lazy-seq
             (let [sum (+ sum n)]
               (cons sum (triangle-seq (inc n) sum))))))

(defn get-divisors [num]
  (flatten
    (for [x (range 1 (inc (Math/sqrt num)))
         :when (zero? (rem num x))]
      (let [y (/ num x)]
        (if (not= x y)
         [x y]
         x)))))

(defn get-triangle-over-n-divisors []
  (first (drop-while #(> n (count (get-divisors %))) (triangle-seq))))
