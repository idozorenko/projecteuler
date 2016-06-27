(ns idozorenko.projecteuler.problem37
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def primes
  (set
    (take 10000
      (for [i (iterate inc 0)
            :let [stri (str i)]
            :when (and
                    (not-any? #(str/includes? stri %) ["0" "4" "6" "8"])
                    (.isProbablePrime (BigInteger. (str i)) 10))]
        stri))))

(defn truncatable? [p]
  (let [cnt (count p)]
    (and
      (every? #(contains? primes %)
        (for [i (range 1 cnt)]
          (subs p i)))
      (every? #(contains? primes %)
        (for [i (range 1 cnt)]
          (subs p 0 (- cnt i)))))))

(defn run []
  (apply +
    (set/difference
      (set
        (for [p primes
              :when (truncatable? p)] (read-string p)))
      #{2 3 5 7})))