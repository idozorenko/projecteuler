(ns idozorenko.projecteuler.problem47
  (:require [clojure.set :as set]))

(defn prime? [i]
  (.isProbablePrime (BigInteger. (str i)) 10))

(defn get-prime-factors [num]
  (set
    (->> (flatten
          (for [x (range 2 (inc (Math/sqrt num)))
                :when (zero? (rem num x))]
            (let [y (/ num x)]
              (if (not= x y)
                [x y]
                x))))
     (filter prime?))))

(defn indices [pred coll]
  (keep-indexed #(when (pred %2) %1) coll))

(defn get-first [x]
  (- (loop [i 10
          dvs []]
     (let [dvs (cons (get-prime-factors i) dvs)
           dsts-length (first (indices #(not= x (count %)) dvs))]
       (if (and (= x (count dvs)) (nil? dsts-length))
         i
         (recur (inc i) (take (or dsts-length (count dvs)) dvs))))) (dec x)))

