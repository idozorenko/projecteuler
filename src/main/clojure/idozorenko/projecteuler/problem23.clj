(ns idozorenko.projecteuler.problem23)

(defn divisors [n]
  (flatten
    (for [x (range 1 (inc (int (Math/sqrt n))))
          :when (and (not= x n) (zero? (rem n x)))]
      (let [d (/ n x)]
        (if (and (not= d n) (not= d x))
          [x d]
          [x])))))

(defn is-sum-of-ab-num? [ab ab-set n]
  (loop [[h & r] ab]
    (if-not h
      false
      (if (contains? ab-set (- n h))
        true
        (recur r)))))

(defn get-abundants [max]
  (->> (range 2 max)
       (map (fn [i] [i (reduce + (divisors i))]))
       (filter #(< (first %) (second %)))
       (map first)))

(defn run []
  (let [abundants (get-abundants 28124)]
    (reduce
      #(cond-> %
               (not (is-sum-of-ab-num? abundants (set abundants) %2))(+ %2))
      0
      (range 1 28124))))