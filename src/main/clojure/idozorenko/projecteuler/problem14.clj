(ns projecteuler.problem14)

(defn collatz-sequence
  ([] (collatz-sequence 1))
  ([n] (lazy-seq
         (let [n (if (odd? n) (+ 1 (* 3 n)) (/ n 2))]
           (cons n (collatz-sequence n))))))

(defn get-max-size [threshold]
  (reduce
    #(if (> (second %) (second %2)) % %2)
    (for [i (range 2 threshold)]
     [i (count (take-while #(not= 1 %) (collatz-sequence i)))])))