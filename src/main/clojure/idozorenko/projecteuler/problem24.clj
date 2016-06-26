(ns idozorenko.projecteuler.problem24
  (:require [clojure.set :as set]))

(defn factorial [n]
  (loop [i 1
         f 1]
    (if (> i n) f
      (recur (inc i) (* f i)))))



(defn closest-multiplier [m threshold]
  (loop [x 1]
    (let [p (* x m)]
      (if (> p threshold)
        (dec x)
        (recur (inc x))))))

(defn get-permutation [n d]
  (loop [i (dec d)
         s 0
         perm ""]
    (if (< i 0)
      perm
      (let [threshold (int (- n s))
            fi (factorial i)
            m (closest-multiplier fi threshold)
            leftover (sort (vec (set/difference
                                  (set (range 0 d))
                                  (set (for [x perm] (read-string (str x)))))))
            m2 (nth leftover m)]
        (recur (dec i) (+ s (* fi m)) (str perm m2))))))

#_(get-permutation (dec 1E6) 10)