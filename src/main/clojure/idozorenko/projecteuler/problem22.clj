(ns idozorenko.projecteuler.problem22
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn get-score [i n]
  (* (inc i) (reduce +
                     (for [c (str/lower-case n)]
                       (- (int c) 96)))))

(defn total-scores []
  (let [s (slurp (io/resource "p022_names.txt"))
        names (->> (str/split s #",")
                   (map #(str/replace % "\"" "")))
        sorted-names (sort names)
        scores (map-indexed get-score sorted-names)]
    (reduce + scores)))

