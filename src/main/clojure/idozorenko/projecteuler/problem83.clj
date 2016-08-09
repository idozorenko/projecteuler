(ns idozorenko.projecteuler.problem83
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn- get-next-point [points matrix]
  (reduce #(if (< (get-in matrix %) (get-in matrix %2)) % %2) points))

(defn- print-path [matrix path]
  (let [m (atom matrix)]
    (doseq [p path]
      (swap! m update-in p #(str "(" % ")")))
    (doseq [j (range 0 (count matrix))]
      (println (get-in @m [j])))))

(def matrix
  (->> (str/split-lines (slurp (io/resource "p082_matrix.txt")))
    (mapv #(let [vals (str/split % #",")]
            (mapv (fn [v] (Integer. v)) vals)))))

(defn solve []
  "dijkstra"
  (let [matrix matrix
        msize (count matrix)
        start-point [0 0]
        final-point [(dec msize) (dec msize)]
        update-m (fn [summatrix pd p visited new-points paths]
                   (if (and (not (contains? visited pd)))
                     (let [p-val (get-in @summatrix p)
                           pd-val (get-in matrix pd)
                           sum (+ p-val pd-val)]
                       (swap! new-points conj pd)
                       (swap! summatrix update-in pd
                         (fn [v] (if (< sum v)
                                   (do
                                     (swap! paths assoc pd (conj (get @paths p) p))
                                     sum) v))))))]
    (let [[summatrix paths]
          (loop [points #{start-point}
                 visited #{}
                 paths (atom {start-point []})
                 summatrix (atom (-> (vec (repeat msize (vec (repeat msize (Integer/MAX_VALUE)))))
                                   (assoc-in start-point (get-in matrix start-point))))]
            (if (empty? points)
              [@summatrix @paths]
              (let [new-points (atom points)
                    [y x :as p] (get-next-point points @summatrix)]
                (let [pd [(inc y) x]]
                  (when (< (inc y) msize)
                    (update-m summatrix pd p visited new-points paths)))

                (let [pu [(dec y) x]]
                  (when (>= (dec y) 0)
                    (update-m summatrix pu p visited new-points paths)))

                (let [pl [y (dec x)]]
                  (when (>= (dec x) 0)
                    (update-m summatrix pl p visited new-points paths)))

                (let [pr [y (inc x)]]
                  (when (< (inc x) msize)
                    (update-m summatrix pr p visited new-points paths)))

                (let [visited (conj visited p)]
                  (recur
                    (set/difference @new-points visited)
                    visited
                    paths
                    summatrix)))))]

      (print-path matrix (conj (get paths final-point) final-point))
      (get-in summatrix final-point))))