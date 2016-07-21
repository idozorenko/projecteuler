(ns idozorenko.projecteuler.problem79
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn permutations [s]
  (lazy-seq
    (if (seq (rest s))
      (apply concat (for [x s]
                      (map #(cons x %) (permutations (remove #{x} s)))))
      [s])))

(defn solve []
  (let [nums (->> (str/split-lines
                   (slurp (io/resource "p079_keylog.txt")))
              (map #(str/split % #"")))
        uniq (set (flatten nums))
       contains-in-order? (fn [s v]
                            (let [r (map #(str/index-of s %) v)]
                              (and (every? identity r) (apply <= r))))]
    (str/join (first
       (drop-while
         (fn [i] (not (every? #(contains-in-order? (str i) %) nums)))
         (permutations uniq))))))