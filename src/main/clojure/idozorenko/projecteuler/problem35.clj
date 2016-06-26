(ns idozorenko.projecteuler.problem35
  (:require [clojure.string :as str]))

(defn get-rotations [n]
  (let [s (str n)]
    (->> (for [i (range (count s))]
           ((comp str/join flatten concat reverse) (split-at i s)))

         (map (comp read-string str/join)))))

(let [primes (set (cons 2
                        (for [i (range 1 1E6)
                              :when (and
                                      (let [exclude ["0" "2" "4" "6" "8"]]
                                        (->> exclude
                                             (map #(not (str/includes? (str i) %)))
                                             (every? true?)))
                                      (.isProbablePrime (BigInteger. (str i)) 10))] i)))]
  (->> (for [i primes] (get-rotations i))
       (filter #(every? (partial contains? primes) %))
       (flatten)
       (set)
       (count)))