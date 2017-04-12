(ns idozorenko.projecteuler.problem80
  (:import (java.math RoundingMode)))

(def SQRT_DIG (bigdec 150))
(def SQRT_PRE (.pow (bigdec 10) (.intValue SQRT_DIG)))

(defn sqrt-newton-raphson [c xn precision]
  (let [fx (.add (.pow xn 2) (.negate c))
        fpx (.multiply xn (bigdec 2))
        xn1 (.divide fx fpx (* 2 (.intValue SQRT_DIG)) RoundingMode/HALF_DOWN)
        xn1 (.add xn (.negate xn1))
        currentSquare (.pow xn1 2)
        currentPrecision (.abs (.subtract currentSquare c))]
    (if (<= (.compareTo currentPrecision precision) -1)
      xn1
      (sqrt-newton-raphson c xn1 precision))))

(defn big-sqrt [c]
  (sqrt-newton-raphson c (bigdec 1) (.divide (bigdec 1) SQRT_PRE)))

(defn factors
  ([n]
   (of [] n 2))
  ([factors n candidate]
   (cond
     (= n 1) factors
     (= 0 (rem n candidate)) (recur (conj factors candidate) (quot n candidate) candidate)
     (> candidate (Math/sqrt n)) (conj factors n)
     :else (recur factors n (inc candidate)))))

(defn digits [n]
  (->> n
    (iterate #(quot % 10))
    (take-while pos?)
    (mapv #(mod % 10))
    rseq))

(defn digits-sum [d]
  (->>
    (-> d
      (.movePointRight 99)
      (.toBigInteger)
      (digits))
    (reduce +)))

(defn solve []
  (->> (range 1 100)
    (filter #(->> %
               (factors)
               (group-by identity)
               (not-every? (fn [[_ v]] (-> (count v) even?)))))
    (map #(-> %
            bigdec
            big-sqrt
            digits-sum))
    (reduce +)))