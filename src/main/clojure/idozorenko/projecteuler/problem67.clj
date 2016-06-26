(ns idozorenko.projecteuler.problem67
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn make-triangle [l]
  (loop [size 1
         rest l
         t []]
    (if (< (count rest) size)
      t
      (let [[head rest] (split-at size rest)]
        (recur (inc size) rest (conj t head))))))

(def triangle
  (make-triangle
    (->> (str/split (slurp (io/resource "p067_triangle.txt")) #"( 0)| |\n|(\r\n0)")
        (map read-string))))

(defn add-up [row1 row2]
  (let [s1 (conj (mapv + row1 row2) 0)
        s2 (cons 0 (mapv + row1 (rest row2)))]
    (map max s1 s2)))

(defn sum-triangle [triangle]
  (if (> 2 (count triangle))
    (first triangle)
    (loop [[row1 row2 & rest] triangle]
      (if-not row2
        row1
        (let [row-sum (add-up row1 row2)]
          (recur (cons row-sum rest)))))))