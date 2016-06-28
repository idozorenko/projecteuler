(ns idozorenko.projecteuler.problem43
  (:require [clojure.string :as str]))

(defn add-trailing-zeros [i]
  (let [stri (str i)
        cnt (count stri)]
    (if (= 3 cnt) stri (str "0" stri))))

(defn reads [s]
  (read-string (if (str/starts-with? s "0") (subs s 1) s)))

(defn find-rest [[divisor & rest-divisors] trail]
  (if (nil? divisor)
    trail
    (for [i (range 0 10)
          :when (not (str/includes? trail (str i)))
          :let [iz (str i (subs trail 0 2))]
          :when (zero? (mod (reads iz) divisor))]
      (find-rest rest-divisors (str i trail)))))

(defn find-divisible [[divisor & rest-divisors]]
  (flatten
    (for [i (range 12 988)
          :let [iz (add-trailing-zeros i)]
          :when (and
                  (= 3 (count (set iz)))
                  (zero? (mod i divisor)))]
     (find-rest rest-divisors iz))))

(time
  (println
    (->> (find-divisible [17 13 11 7 5 3 2 1])
     (map reads)
     (reduce +))))