(ns idozorenko.projecteuler.problem42
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn triangle-numbers
  ([] (triangle-numbers 1))
  ([n] (lazy-seq (cons (int (* (/ n 2) (inc n))) (triangle-numbers (inc n))))))

(let [words (->> (str/split (slurp (io/resource "p042_words.txt")) #",")
              (map #(subs % 1 (dec (count %)))))
      triangle? (fn [x] (= x (first (drop-while #(> x %) (triangle-numbers)))))]
  (count
    (->> words
      (filter #(let [sum (reduce + (for [c (str/lower-case %)] (- (int c) 96)))]
                (triangle? sum))))))