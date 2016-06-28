(ns idozorenko.projecteuler.problem41
  (:require [clojure.string :as str]))

(defn permutations [s]
  (lazy-seq
    (if (seq (rest s))
      (apply concat (for [x s]
                      (map #(cons x %) (permutations (remove #{x} s)))))
      [s])))

(apply max
  (cons 0
    (for [n (range 2 10)]
      (apply max
        (cons 0
          (for [i (permutations (str/join (range 1 n)))
                :let [stri (str/join i)]
                :when (.isProbablePrime (BigInteger. stri) 10)]
            (read-string stri)))))))
