(ns idozorenko.projecteuler.problem58)

(defn size-len []
  (dec
    (* 2
      (let [prime? #(.isProbablePrime (BigInteger. (str %)) 10)]
       (loop [lev 1
              total 1
              primes 0
              last-num 1]
         (let [c (double (/ primes total))]
           (if (and (< 1 lev) (< c 0.1))
             lev
             (let [nums (for [i (range 1 5)] (+ last-num (* i lev 2)))
                   total (+ total (count nums))
                   primes (+ primes (count (filter prime? nums)))]
               (recur (inc lev) total primes (last nums))))))))))