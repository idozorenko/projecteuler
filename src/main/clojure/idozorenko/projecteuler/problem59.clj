(ns idozorenko.projecteuler.problem59
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn solve []
  (let [cipher (->> (str/split (slurp (io/resource "p059_cipher.txt")) #",")
                 (map #(Integer. %)))
        codes (for [x (range 97 123)
                    y (range 97 123)
                    z (range 97 123)]
                [x y z])]
    (let [res (->> (for [i codes]
                     (let [decrypted (->> cipher (map-indexed (fn [idx c] (bit-xor c (nth i (mod idx 3))))))]
                   [decrypted (->> decrypted
                                (filter #(and (> 123 %) (< 65 %)))
                                (count))]))
            (sort-by second)
            (last)
            (first))]
      (println (->> res
                 (map char)
                 (str/join)))
      (->> res
        (reduce +)))))