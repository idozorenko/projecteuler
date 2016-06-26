(ns idozorenko.projecteuler.problem36
  (:require [clojure.string :as str]))

(let [palindromic? #(= (str/reverse %) %)]
  (reduce
    #(cond-> % (and (palindromic? (str %2))
                    (palindromic? (Integer/toBinaryString %2)))
             (+ %2))
    (range 1 (inc 1E6))))