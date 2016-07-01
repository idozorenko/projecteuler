(ns idozorenko.projecteuler.problem54
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def ranks
  (into {}
    (concat
      (->> (range 2 10)
        (map (fn [i] [(keyword (str i)) (dec i)])))
      {:T 9 :J 10 :Q 11 :K 12 :A 13})))

(defn parse-card [s]
  (let [[rs ss] s]
    {:rank (-> rs (str) (keyword) (ranks))
     :suit (-> ss (str) (keyword))}))

(defn- get-sims [h]
  (->> h
    (map :rank)
    (group-by identity)
    (map (fn [[x y]] [x (count y)]))
    (group-by second)
    (map (fn [[x y]] [x (set (map first y))]))
    (into {})))

(defn high-card? [_ _] true)

(defn one-pair? [_ sims]
  (= 1 (count (get sims 2))))

(defn two-pairs? [_ sims]
  (= 2 (count (get sims 2))))

(defn three-of-a-kind? [_ sims]
  (= 1 (count (get sims 3))))

(defn four-of-a-kind? [_ sims]
  (= 1 (count (get sims 4))))

(defn flush? [h _]
  (boolean (some #(= 5 (count (second %))) (group-by :suit h))))

(defn full-house? [_ sims]
  (and (= 1 (count (get sims 2))) (= 1 (count (get sims 3)))))

(defn straight? [h _]
  (let [straight? (fn [h]
                    (let [sorted-ranks (sort (map :rank h))
                          ex (take (count h) (iterate inc (first sorted-ranks)))]
                      (= sorted-ranks ex)))]
    (or
     (straight? h)
     (straight? (map #(update-in % [:rank] mod 13) h)))))

(defn straight-flush? [h sims]
  (and (straight? h sims) (flush? h sims)))

(def combp
  (map-indexed
    (fn [i c] {:p c :rank i})
    [high-card?
     one-pair?
     two-pairs?
     three-of-a-kind?
     straight?
     flush?
     full-house?
     four-of-a-kind?
     straight-flush?]))

(defn combination [h]
  (let [sims (get-sims h)]
    (loop [[{:keys [p rank]} & crest] (reverse combp)]
      (if (p h sims)
        {:h h :rank rank :sims sims}
        (recur crest)))))

(defn compare-sims [sims1 sims2]
  (loop [[[[_ c1] & crest1]
          [[_ c2] & crest2]] [(reverse (sort-by key sims1)) (reverse (sort-by key sims2))]]
    (if (or (nil? c1) (nil? c2))
      0
      (let [common (set/intersection c1 c2)
            c1 (set/difference c1 common)
            c2 (set/difference c2 common)
            cmp (if (or (empty? c1) (empty? c2)) 0
                  (compare
                   (apply max c1)
                   (apply max c2)))]
        (if (not (zero? cmp))
          cmp
          (recur [crest1 crest2]))))))


(defn winner [h1 h2]
  (let [{rank1 :rank sims1 :sims} (combination h1)
        {rank2 :rank sims2 :sims} (combination h2)]
    (let [cmp (compare rank1 rank2)]
      (if (zero? cmp)
        (compare-sims sims1 sims2)
        cmp))))


(defn run []
  (->> (let [games (-> (slurp (io/resource "p054_poker.txt"))
                     (str/split-lines))]
         (for [game games]
           (let [cards (->> (str/split game #" ")
                         (map parse-card))
                 [p1hand p2hand] (split-at 5 cards)]
             (winner p1hand p2hand))))
    (group-by identity)
    (map (fn [[x y]] [x (count y)]))))