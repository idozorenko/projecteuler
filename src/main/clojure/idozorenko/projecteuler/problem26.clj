(ns idozorenko.projecteuler.problem26
  (:require [clojure.string :as str]))

(defn max-defined [res]
  (->> res
       (map-indexed (fn [i x] [i x]))
       (reduce (fn [x y]
                 (let [m (if (< (second (second x)) (second (second y))) y x)]
                   m)))))

(defn defined [res]
  (->> res
       (map-indexed (fn [i x] [i x]))
       (filter (comp first second))
       (map first)
       (set)))

(defn divisors [n]
  (flatten
    (for [x (range 1 (inc (int (Math/sqrt n))))
          :when (and (not= x n) (zero? (rem n x)))]
      (let [d (/ n x)]
        (if (and (not= d n) (not= d x))
          [x d]
          [x])))))

(defn cycle2? [cycle s threshold]
  (loop [i 0]
    (let [j (* i (count cycle))
          cycle-end (+ j (count cycle))]
      (if (or (> i threshold) (> cycle-end (count s)))
        true
        (let [idx (str/index-of s cycle j)]
          (if (not= j idx)
            false
            (recur (inc i))))))))

(defn contains-cycle? [fraction-str start-from]
  (loop [i (inc start-from)]
    (if (<= (/ (count fraction-str) 2) i)
      nil
      (let [[cycle rest] (split-at i fraction-str)
            cycle (str/join cycle)
            rest (str/join rest)]
        (if (cycle2? cycle rest 5)
          cycle
          (recur (inc i)))))))

(defn get-cycle-length [i fraction-str precision exclude start-from]
  (if (or (= 1 (count (divisors (inc i))))
        (> (dec precision) (count fraction-str)))
    ["" 0]
    (if (contains? exclude i)
     [nil 0]
     (loop [s fraction-str]
       (if (> (- (count fraction-str) (count s)) 15)
         [nil 0]
         (if-let [cycle (contains-cycle? s start-from)]
           [cycle (count cycle)]
           (recur (rest s))))))))

(defn get-cycles [precision max-denominator exclude start-from]
  (let [numerator (bigdec 1)
        decimal-fractions (for [denominator (range 2 (inc max-denominator))]
                            (let [dec (with-precision precision
                                        (/ numerator denominator))
                                  sdec (str dec)
                                  sdec (subs sdec 2 (- (count sdec) 1))]
                              sdec))]
    #_(time (doall decimal-fractions))
    (let [cycles (->> decimal-fractions
                      (map-indexed #(get-cycle-length % %2 precision exclude start-from)))]
      (doall cycles))))


(defn get-longest-cycle-gradually []
  (let [precisions [50 100 500 1000 5000]
        dfnd (atom [])
        max-cycle (atom 0)
        max-cycle-index (atom -1)]
    (doseq [precision precisions]
      (let [cycles (get-cycles precision 1000 (set @dfnd) @max-cycle)]
        (println "size >> " (count cycles) " cycles >> " cycles)
        (swap! dfnd concat (defined cycles))
        (swap! max-cycle max (apply max (map second cycles)))
        (reset! max-cycle-index (max-defined cycles))
        (println "precision >> " precision " max cycle >> " @max-cycle " defined >> " (count (set @dfnd)) " index >> " (inc (first @max-cycle-index)))))))
