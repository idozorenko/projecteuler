(ns idozorenko.projecteuler.problem32
  (:require [clojure.string :as str]))

(reduce +
  (set (let [pandigital? (fn [s] (and (= 9 (count s)) (not (str/includes? s "0")) (= (count s) (count (set s)))))]
         (for [i (range 1 99)
               j (range i 9877)
               :let [p (* i j)]
               :when (pandigital? (str i j p))]
           p))))
