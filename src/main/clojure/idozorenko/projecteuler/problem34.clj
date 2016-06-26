(ns idozorenko.projecteuler.problem34
  (:require [clojure.string :as str]))

(defn factorial [n]
  (apply * (range 1 (inc n))))

(def factorials
  (into {}
        (for [i (range 0 10)]
          [(keyword (str i)) (factorial i)])))

(->> (range 3 (* 7 (:9 factorials)))
     (reduce
       (fn [s i] (cond-> s (= i (reduce #(+ % (-> %2 str keyword factorials)) 0 (str i))) (+ i))) 0))