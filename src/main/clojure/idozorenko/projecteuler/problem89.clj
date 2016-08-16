(ns idozorenko.projecteuler.problem89
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def nums
  {"I" 1
   "V" 5
   "X" 10
   "L" 50
   "C" 100
   "D" 500
   "M" 1000})

(def diffs
  {"IV" 4
   "IX" 9
   "XL" 40
   "XC" 90
   "CD" 400
   "CM" 900})

(def arab-to-roman-map
  (apply sorted-map-by (concat [(comparator #(> % %2))]
                         (flatten
                           (vec
                             (set/map-invert
                               (set/union nums diffs)))))))

(defn to-roman [x]
  (loop [x x
         [[a r] & rest] (vec arab-to-roman-map)
         s ""]
    (if (>= 0 x)
      s
      (let [[x s] (loop [x x s s]
                    (if (< x a)
                      [x s]
                      (recur (- x a) (str s r))))]
        (recur x rest s)))))

(defn to-arab [s]
  (let [diff-keys (keys diffs)
        reg-part (str/join "|" diff-keys)
        reg-pattern (re-pattern (str "((?<=" reg-part ")|(?=" reg-part "))"))]
    (->> (str/split s reg-pattern)
      (map (fn [s]
             (if-let [x (get diffs s)]
               x
               (->> (str/split s #"")
                 (map nums)
                 (reduce +)))))
      (reduce +))))

(defn roman-optimized-diff [roman]
  (let [arab (to-arab roman)
        new-roman (to-roman arab)]
    (- (count roman) (count new-roman))))

(defn solve []
  (->> (-> (slurp (io/resource "p089_roman.txt"))
         (str/split-lines))
    (map roman-optimized-diff)
    (reduce +)))