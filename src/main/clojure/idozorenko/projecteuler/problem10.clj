(ns projecteuler.problem10)

(defn- is-prime? [x]
  (or (= (mod x 2) 0)
    (= (mod x 3) 0)
    (.isProbablePrime (BigInteger. (str x)) 7)))

(defn sum-of-primes [max]
  (reduce + (concat [2]
              (take-while #(< % max)
                (filter is-prime?
                  (take-nth 2
                    (range 1 Integer/MAX_VALUE)))))))