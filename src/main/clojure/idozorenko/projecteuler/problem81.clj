(ns idozorenko.projecteuler.problem81
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def matrix
  (->> (str/split-lines (slurp (io/resource "p081_matrix.txt")))
    (mapv #(let [vals (str/split % #",")]
           (mapv (fn [v] (Integer. v)) vals)))))

(defn solve []
  (let [matrix matrix
        msize (count matrix)
        min-path-matrix
        (loop [points [[0 0]]
               summatrix (atom matrix)
               i 0]
          (if (empty? points)
            @summatrix
            (let [new-points (atom #{})
                  current-matrix @summatrix]
              (doseq [[x y :as p] points]
                (if (< (inc x) msize)
                  (let [pr [(inc x) y]
                        p-val (get-in current-matrix p)
                        pr-val (get-in current-matrix pr)
                        sum (+ p-val pr-val)]
                    (swap! new-points conj pr)
                    (swap! summatrix update-in pr (fn [v] (if (or (= v pr-val) (< sum v)) sum v)))))
                (if (< (inc y) msize)
                  (let [pd [x (inc y)]
                        p-val (get-in current-matrix p)
                        pd-val (get-in current-matrix pd)
                        sum (+ p-val pd-val)]
                    (swap! new-points conj pd)
                    (swap! summatrix update-in pd (fn [v] (if (or (= v pd-val) (< sum v)) sum v))))))
              (recur @new-points summatrix (inc i)))))]
    (get-in min-path-matrix [(dec msize) (dec msize)])))