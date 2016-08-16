(ns idozorenko.projecteuler.problem102
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def triangles
  (->> (str/split-lines (slurp (io/resource "p102_triangles.txt")))
    (map #(let [[x1 y1 x2 y2 x3 y3] (->> (str/split % #",")
                                      (map (fn [s] (Integer. s))))]
           [[x1 y1] [x2 y2] [x3 y3]]))))

(defn contains-point? [[[x1 y1] [x2 y2] [x3 y3]] [xp yp]]
  (let [a (/ (+ (* (- y2 y3) (- xp x3)) (* (- x3 x2) (- yp y3)))
            (+ (* (- y2 y3) (- x1 x3)) (* (- x3 x2) (- y1 y3))))
        b (/ (+ (* (- y3 y1) (- xp x3)) (* (- x1 x3) (- yp y3)))
            (+ (* (- y2 y3) (- x1 x3)) (* (- x3 x2) (- y1 y3))))
        g (- 1.0 a b)]
    (and (< 0 a) (< 0 b) (< 0 g))))

(defn solve []
  (count
    (->> triangles
     (map #(contains-point? % [0 0]))
     (filter true?))))