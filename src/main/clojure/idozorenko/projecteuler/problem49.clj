(ns idozorenko.projecteuler.problem49)

(def primes
  (drop-while #(> 1000 %)
    (take-while #(> 10000 %)
      (for [i (iterate inc 1)
            :when (.isProbablePrime (BigInteger. (str i)) 10)] i))))

(defn run []
  (->> primes
    (group-by #(set (str %)))
    (filter #(< 3 (count (second %))))
    (vals)
    (map
      #(for [y (let [x %]
                (for [i (range 0 (count x))
                      j (range (inc i) (count x))
                      k (range (inc j) (count x))]
                  [(nth x i) (nth x j) (nth x k)]))
            :let [[a b c] y]
            :when (= (- b a) (- c b))]
        y))
    (filter not-empty)))