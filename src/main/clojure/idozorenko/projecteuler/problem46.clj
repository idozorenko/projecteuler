(ns idozorenko.projecteuler.problem46)

(defn prime? [i]
  (.isProbablePrime (BigInteger. (str i)) 10))

(def primes
  (cons 1
    (take-while #(> 10000 %)
     (for [i (iterate inc 1)
           :when (prime? i)] i))))

(defn goldbach? [n]
  (let [primes (take-while #(< % n) primes)
        twice-squares (take-while #(< % n) (map #(* 2 % %) (iterate inc 1)))]
    (some #(= n (apply + %))
      (for [p primes
            ts twice-squares]
        [p ts]))))

(defn first-non-goldbach []
  (first (drop-while #(or (prime? %) (goldbach? %))
           (iterate #(+ 2 %) 3))))