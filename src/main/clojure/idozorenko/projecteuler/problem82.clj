(ns idozorenko.projecteuler.problem82
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]))

(def matrix
  (->> (str/split-lines (slurp (io/resource "p082_matrix.txt")))
    (mapv #(let [vals (str/split % #",")]
            (mapv (fn [v] (Integer. v)) vals)))))

(defn- get-next-point [points matrix]
  (reduce #(if (< (get-in matrix %) (get-in matrix %2)) % %2) points))

(defn solve []
  (let [matrix matrix
        msize (count matrix)]
    (let [col (atom (->> (range 0 msize)
                      (mapv #(get-in matrix [% 0]))))]

      (doseq [i (range 1 msize)]
        (swap! col update 0 (fn [v] (+ v (get-in matrix [0 i]))))
        (doseq [j (range 1 msize)]
          (swap! col update j
            (fn [v] (min (+ (nth @col (dec j)) (get-in matrix [j i]))
                      (+ v (get-in matrix [j i]))))))

        (doseq [j (range (- msize 2) -1 -1)]
          (swap! col update j
            (fn [v] (min (+ (nth @col (inc j)) (get-in matrix [j i]))
                      v)))))
      (apply min @col))))

(defn solve2 []
  "dijkstra"
  (let [matrix matrix
        msize (count matrix)]
    (reduce min
      (for [start-y (range 0 msize)]
        (let [[min-path-matrix paths]
              (loop [points #{[start-y 0]}
                     visited #{}
                     paths (atom {[start-y 0] []})
                     summatrix (atom matrix)
                     i 0]
                (pprint {:p points :v visited})
                (if (empty? points)
                  [@summatrix @paths]
                  (let [new-points (atom points)
                        current-matrix @summatrix
                        [y x :as p] (get-next-point points current-matrix)]

                    (let [pd [(inc y) x]]
                      (if (and (not (contains? visited pd)) (< (inc y) msize))
                       (let [p-val (get-in current-matrix p)
                             pd-val (get-in current-matrix pd)
                             sum (+ p-val pd-val)]
                         (swap! new-points conj pd)
                         (swap! summatrix update-in pd
                           (fn [v] (if (or (= v pd-val) (< sum v))
                                     (do
                                       (swap! paths assoc pd (conj (get @paths p) p))
                                       sum) v))))))

                    (let [pu [(dec y) x]]
                      (if (and (not (contains? visited pu)) (>= (dec y) 0))
                       (let [p-val (get-in current-matrix p)
                             pu-val (get-in current-matrix pu)
                             sum (+ p-val pu-val)]
                         (swap! new-points conj pu)
                         (swap! summatrix update-in pu
                           (fn [v] (if (or (= v pu-val) (< sum v))
                                     (do
                                       (swap! paths assoc pu (conj (get @paths p) p))
                                       sum) v))))))

                    (let [pr [y (inc x)]]
                      (if (and (not (contains? visited pr)) (< (inc x) msize))
                       (let [p-val (get-in current-matrix p)
                             pr-val (get-in current-matrix pr)
                             sum (+ p-val pr-val)]
                         (swap! new-points conj pr)
                         (swap! summatrix update-in pr
                           (fn [v] (if (or (= v pr-val) (< sum v))
                                     (do
                                       (swap! paths assoc pr (conj (get @paths p) p))
                                       sum) v))))))

                    (let [visited (conj visited p)]
                      (recur
                        (set/difference @new-points visited)
                        visited
                        paths
                        summatrix
                        (inc i))))))]
          (let [last-row-values (for [start-y (range 0 msize)]
                                  (let [p [start-y (dec msize)]
                                        value (get-in min-path-matrix p)]
                                    (println value (get paths p))
                                    value))]

            (reduce min
             last-row-values)))))))


(defn solve3 []
  "floyd-warshall"
  (let [matrix matrix
        msizey (count matrix)
        msizex (count (get matrix 0))
        wsize (* msizex msizey)
        to-w (fn [i j] (+ i (* j msizex)))
        get-m (fn [i j] (get-in matrix [j i]))
        W (atom (vec (repeat wsize (vec (repeat wsize (Integer/MAX_VALUE))))))]
    (doseq [j (range 0 msizey)
            i (range 0 msizex)]

      ;self
      (let [val (get-m i j)]
        (swap! W assoc-in [(to-w i j) (to-w i j)] val))
      ;to top
      (when (> j 0)
        (let [top-val (get-m i (dec j))]
          (swap! W assoc-in [(to-w i j) (to-w i (dec j))] top-val)))
      ;to bottom
      (when (< j (dec msizey))
        (let [bottom-val (get-m i (inc j))]
          (swap! W assoc-in [(to-w i j) (to-w i (inc j))] bottom-val)))
      ; to right
      (when (< i (dec msizex))
        (let [right-val (get-m (inc i) j)]
          (swap! W assoc-in [(to-w i j) (to-w (inc i) j)] right-val))))

    (println "created W matrix of size " wsize)

    (let [last-k (atom 0)]
      (doseq [k (range 0 wsize)
             i (range 0 wsize)
             j (range 0 wsize)]
       (let [Wik (get-in @W [i k])
             Wkj (get-in @W [k j])]
         (when (and (not= @last-k k))
           (reset! last-k k)
           (println k))
         (swap! W update-in [i j] (fn [v] (min v (+ Wik Wkj)))))))

    ;(doseq [i (range 0 wsize)
    ;        j (range 0 wsize)]
    ;  (swap! W update-in [i j] #(if (= % Integer/MAX_VALUE) nil %)))
    ;
    ;(pprint @W)
    (let [first-column (for [y (range 0 msizey)] y)
          last-column (for [y (range 0 msizey)] y)]
      (let [vals (for [start first-column
                       end last-column]
                   (+ (get-in matrix [start end])
                     (get-in @W [(to-w 0 start) (to-w (dec msizex) end)])))]
        (reduce min vals))
      )
    ))