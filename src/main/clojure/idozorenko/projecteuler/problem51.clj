(ns idozorenko.projecteuler.problem51
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(defonce primes
  (for [i (range 1 1E6)
        :when (.isProbablePrime (BigInteger. (str i)) 10)] i))

(defonce primes-set
  (set primes))

(defn replace-yields-prime-family? [digits ps len]
  (let [f (fn [digits ps c]
            (loop [[d & ds] digits
                   s ps]
              (if (nil? d)
                (read-string s)
                (let [st (take (dec d) s)
                      end (drop d s)]
                  (recur ds (str (str/join st) c (str/join end)))))))]
    (let [x (for [i (range (if (some #(= 1 %) digits) 1 0) 10)] (f digits ps (str i)))]
      (= len (count (->> x
                      (map #(contains? primes-set %))
                      (filter true?)))))))

(defn run []
  (loop [[prime & prest] (take 100000 primes)]
    (if (or (nil? prime)
          (let [ps (str prime)
                digits (let [x (range 1 (count ps))]
                         (mapcat
                           #(combo/combinations x %)
                           (range 2 (inc (count x)))))]
            (some #(replace-yields-prime-family? % ps 8) digits)))
      prime
      (recur prest))))