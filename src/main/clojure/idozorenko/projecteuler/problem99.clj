(ns idozorenko.projecteuler.problem99
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn solve []
  (let [pairs (-> "p099_base_exp.txt"
                (io/resource)
                (slurp)
                (str/split-lines)
                (->>
                  (map (fn [s]
                         (map #(Integer. %) (str/split s #","))))))]
    (->> pairs
      (map-indexed
        (fn [i [a b]]
          [(inc i) (* b (Math/log a))]))
      (reduce (fn [a b] (if (> (second a) (second b)) a b)))
      (first))))