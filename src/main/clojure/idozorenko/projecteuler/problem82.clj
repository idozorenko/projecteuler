(ns idozorenko.projecteuler.problem82
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]))

(def matrix
  (->> (str/split-lines (slurp (io/resource "p082_matrix.txt")))
    (mapv #(let [vals (str/split % #",")]
            (mapv (fn [v] (Integer. v)) vals)))))

(defn solve []
  (let [matrix matrix
        msize (count matrix)]
    (let [col (atom (->> (range 0 msize)
                      (mapv #(get-in matrix [% 0]))))]

      (doseq [i (range 1 msize)]
        (swap! col update 0 (fn [v] (+ v (get-in matrix [0 i]))))
        (doseq [j (range 1 msize)]
          (swap! col update j
            (fn [v] (min (+ (nth @col (dec j)) (get-in matrix [j i]))
                      (+ v (get-in matrix [j i]))))))

        (doseq [j (range (- msize 2) -1 -1)]
          (swap! col update j
            (fn [v] (min (+ (nth @col (inc j)) (get-in matrix [j i]))
                      v)))))
      (apply min @col))))