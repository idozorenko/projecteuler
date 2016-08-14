(ns idozorenko.projecteuler.problem96
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))


(def full-row-set (set (range 1 10)))

(def sudokus
  (let [s (slurp (io/resource "p096_sudoku.txt"))]
    (->> s
      (str/split-lines)
      (partition 10)
      (map #(let [s (drop 1 %)]
             (mapv
               (fn [row]
                 (mapv
                   (fn [v] (Integer. v))
                   (str/split row #"")))
               s))))))

(defn pprint [sudoku [i j]]
  (println "=========[" i " " j "]=========")
  (println)
  (doseq [row-group (partition 3 sudoku)]
    (doseq [row row-group]
      (doseq [col-group (partition 3 row)]
        (doseq [value col-group]
          (print value " "))
        (print " "))
      (println))
    (println))
  (println "==========================="))

(defn boxes [sudoku]
  (->> sudoku
    (partition 3)
    (mapcat (fn [x] (let [y (->> x (flatten) (partition 3))]
                      (for [i (range 3)]
                        (vec (mapcat #(nth y %) (take-nth 3 (drop i (range 9)))))))))
    (vec)))

(defn rows [sudoku]
  sudoku)

(defn cols [sudoku]
  (apply mapv vector sudoku))

(defn- valid-group? [l]
  (let [g (group-by identity l)]
    (every? #(or (zero? (first %)) (= 1 (count %))) (vals g))))

(defn defined-value? [v]
  (not (zero? v)))

(defn- defined? [l]
  (= (set l) full-row-set))

(defn- defined [s]
  (->> s
    (mapv #(set (filter (comp not zero?) %)))))

(defn valid? [sudoku]
  (and
    (every? valid-group? (boxes sudoku))
    (every? valid-group? (rows sudoku))
    (every? valid-group? (cols sudoku))))

(defn solved? [sudoku]
  (and
    (every? defined? (boxes sudoku))
    (every? defined? (rows sudoku))
    (every? defined? (cols sudoku))))

(defn box-idx [[i j]]
  (+ (* 3 (int (/ i 3))) (int (/ j 3))))

(defn from-box [i box-idx]
  (let [box-row-idx (int (/ i 3))
        box-col-idx (mod i 3)]
    [(+ (* box-row-idx 3) (int (/ box-idx 3))) (+ (* box-col-idx 3) (mod box-idx 3))]))

(defn deduce-only-choice [sudoku]
  (letfn [(get-one-left [row]
            (when (= 1 (count (filter zero? row)))
              [(.indexOf row 0) (first (set/difference full-row-set (set row)))]))]
    (first
      (->> (range 9)
        (map (fn [i]
               (let [[col-idx value] (get-one-left (nth (rows sudoku) i))]
                 (if value
                   [[i col-idx] value]
                   (let [[row-idx value] (get-one-left (nth (cols sudoku) i))]
                     (if value
                       [[row-idx i] value]
                       (let [[box-idx value] (get-one-left (nth (boxes sudoku) i))]
                         (if value
                           [(from-box i box-idx) value]
                           nil))))))))
       (filter second)))))

(defn deduce-only-choice-left-in-box [sudoku]
  (let [boxes (boxes sudoku)
        cols (cols sudoku)
        rows (rows sudoku)]
   (letfn [(filter-exclude [i box]
             (let [x (->> box
                     (keep-indexed (fn [i v] (if (not (defined-value? v)) i nil)))
                     (map (fn [box-idx]
                            (let [[row-idx col-idx :as cell] (from-box i box-idx)]
                              [cell (set/difference full-row-set
                                      (set/union
                                       (set box)
                                       (set (nth rows row-idx))
                                       (set (nth cols col-idx))))]))))]
               (let [x (set x)]
                 (first
                   (->> x
                    (map (fn [xs]
                           [(first xs) (first (set/difference (second xs)
                                          (apply set/union (map second (remove #{xs} x)))))]))
                    (filter second))))))]
     (first
       (->> boxes
         (map-indexed filter-exclude)
         (filter identity))))))

(defn deduce-only-choice-left-in-row [sudoku]
  (letfn [(check [sudoku]
             (let [defined-col-values (defined (cols sudoku))
                   defined-box-values (defined (boxes sudoku))
                   not-defined-col-idx (->> (rows sudoku)
                                         (mapv #(keep-indexed (fn [idx v] (if (defined-value? v) nil idx)) %)))
                   filter-exclude
                   (fn [[row-idx col-idx :as cell]]
                     (let [box-i (box-idx cell)
                           other-col-idx (partition-by #(int (/ % 3))
                                           (sort (set/difference (set (nth not-defined-col-idx row-idx)) #{col-idx})))
                           defined-col-values (->> other-col-idx
                                                (map-indexed
                                                  (fn [i x]
                                                    (let [col-values (keep-indexed (fn [idx value] (if (contains? (set x) idx) value nil)) defined-col-values)
                                                          current-box-idx (box-idx [row-idx (first x)])]
                                                      (cond-> (apply set/intersection col-values)
                                                        (not= box-i current-box-idx) (set/union (nth defined-box-values current-box-idx)))))))

                           same-col-values (if (empty? defined-col-values) #{}
                                             (apply set/intersection defined-col-values))
                           not-allowed (set/union
                                         (nth defined-box-values box-i)
                                         (set (nth (cols sudoku) col-idx))
                                         (set (nth (rows sudoku) row-idx)))]
                       [cell (first (set/difference same-col-values not-allowed))]))]

               (first
                 (->> (for [i (range 9) j (range 9)] [i j])
                   (filter #(not (defined-value? (get-in sudoku %))))
                   (map filter-exclude)
                   (filter second)))))]
    (let [result (check (rows sudoku))]
      (if-not (second result)
        (let [[[col-idx row-idx] value] (check (cols sudoku))]
          [[row-idx col-idx] value])
        result))))

(defn deduce-next-cell [sudoku]
  (let [fns [deduce-only-choice-left-in-row
             deduce-only-choice-left-in-box
             deduce-only-choice]]
    (first (->> fns
             (map #(% sudoku))
             (filter second)))))

(defn solve-by-deduction [sudoku]
  (loop [sudoku sudoku]
    (if (not (valid? sudoku))
      [:not_valid sudoku]
      (if (solved? sudoku)
        [:solved sudoku]
        (let [[cell value] (deduce-next-cell sudoku)]
          (if (nil? value)
            [:no_solution sudoku]
            (let [sudoku (assoc-in sudoku cell value)]
              (pprint sudoku cell)
              (recur sudoku))))))))


(defn get-best-guess [sudoku failed-guesses]
  (let [boxes (->> (boxes sudoku)
                (map-indexed (fn [i v] [i v]))
                (filter #(not (every? (comp not zero?) (second %))))
                (sort-by #(count (filter zero? (second %)))))]
    (first
      (->> boxes
        (keep (fn [box]
                (let [cell (from-box (-> box first) (first (keep-indexed (fn [i v] (when (not (defined-value? v)) i)) (-> box second))))
                      guess (first
                              (set/difference full-row-set
                                (set (-> box second))
                                (get failed-guesses cell)))]
                  (when guess [cell guess]))))))))


(defn try-guess-next-cell [sudoku guesses]
  (println "> " guesses)
  (loop [failed-guesses {}]
    (let [[cell guess] (get-best-guess sudoku failed-guesses)]
      (println cell guess)
      (if-not guess
        [:no_solution nil]
        (let [sudoku (assoc-in sudoku cell guess)
              [result sudoku] (solve-by-deduction sudoku)]
          (if (= result :solved)
            sudoku
            (if-let [result (if (= result :no_solution)
                              (let [result (try-guess-next-cell sudoku (conj guesses [cell guess]))]
                                (if (not= :no_solution (first result))
                                  result)))]
              result
              (recur (update-in failed-guesses [cell] (fn [v] (conj (set v) guess)))))))))))

(defn solve-with-heuristics [sudoku]
  (let [[result sudoku] (solve-by-deduction sudoku)]
    (if (= :not_valid sudoku)
      (println "NOT VALID!")
      (if (not= :no_solution result)
        sudoku
        (try-guess-next-cell sudoku [])))))
