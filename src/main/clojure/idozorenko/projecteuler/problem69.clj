(ns idozorenko.projecteuler.problem69
  (:require [clojure.set :as set]))

(defn solve []
  (loop [[p & rest] (filter #(.isProbablePrime (BigInteger. (str %)) 7)
                      (iterate inc 1))
         product 1]
    (let [new-product (* product p)]
      (if (> new-product 1E6)
        product
        (recur rest new-product)))))