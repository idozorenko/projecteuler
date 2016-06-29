(ns idozorenko.projecteuler.problem38
  (:require [clojure.string :as str]))

(let [pandigital? (fn [s] (and (not (str/includes? s "0")) (= (count s) (count (set s)))))]
  (apply max
    (->> (for [i (range 1 10000)]
           (loop [pc ""
                  j 1]
             (if (<= 9 (count pc))
               (if (and (= 9 (count pc)) (pandigital? pc)) (read-string pc) false)
               (let [s (str (* i j))]
                 (recur (str pc s) (inc j))))))
      (filter identity))))