(ns idozorenko.projecteuler.problem50)

(def primes
  (for [i (range 1 1E6)
        :when (.isProbablePrime (BigInteger. (str i)) 10)]
    i))

(def primes-set
  (set primes))

(apply max-key
  (cons :length
    (for [k (range 1 (count primes))]
      (loop [i k
             s 0
             p {:sum 0 :length 0}]
        (if (or (> s 1E6) (> i 1000))
          p
          (let [prime (nth primes i)
                s (+ s prime)
                p (if (contains? primes-set s) {:sum s :length (- i k)} p)]
            (recur (inc i) s p)))))))