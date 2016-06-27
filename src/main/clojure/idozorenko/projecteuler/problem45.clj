(ns idozorenko.projecteuler.problem45)

(defn triangle-numbers
  ([] (triangle-numbers 1))
  ([n] (lazy-seq (cons (* (/ n 2) (inc n)) (triangle-numbers (inc n))))))

(defn pentagonal-numbers
  ([] (pentagonal-numbers 1))
  ([n] (lazy-seq (cons (* (/ n 2) (dec (* 3 n))) (pentagonal-numbers (inc n))))))

(defn hexagonal-numbers
  ([] (hexagonal-numbers 1))
  ([n] (lazy-seq (cons (* n (dec (* n 2))) (hexagonal-numbers (inc n))))))

(defn run []
  (loop [[t & ts :as tn] (drop 285 (triangle-numbers))
         [p & ps :as pn] (drop 165 (pentagonal-numbers))
         [h & hs :as hn] (drop 143 (hexagonal-numbers))]
    (if (= t p h)
      t
      (let [min (min t p h)]
        (recur
          (if (= min t) ts tn)
          (if (= min p) ps pn)
          (if (= min h) hs hn))))))