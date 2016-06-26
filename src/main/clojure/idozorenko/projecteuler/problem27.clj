(ns idozorenko.projecteuler.problem27)

(defn f [n a b]
  (+ (* n n) (* a n) b))

(def primes
  (set
    (cons 1
         (take 10000
               (for [i (iterate inc 0)
                     :when (.isProbablePrime (BigInteger. (str i)) 10)]
                 i)))))

(defn get-count-of-consecutive-primes [[a b]]
  (count
    (for [n (iterate inc 0)
          :while (let [res (f n a b)]
                   (contains? primes res))]
      n)))

(defn run []
  (-> (reduce
        #(let [cnt (get-count-of-consecutive-primes %2)]
          (if (> cnt (second %)) [%2 cnt] %))
        [nil 0]
        (for [a (range -1000 1001)
              b (range -1000 1001)]
          [a b]))
      ((comp (partial apply *) first))))