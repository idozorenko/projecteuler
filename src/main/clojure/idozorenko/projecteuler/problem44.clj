(ns idozorenko.projecteuler.problem44)

(defn pentagonal [n]
  (* (/ n 2) (dec (* 3 n))))

(defn pentagonal? [p]
  (let [x (/ (+ 1 (Math/sqrt (+ 1 (* 24 p)))) 6)]
    (= x (Math/floor x))))

(defn pentagonal-numbers
  ([] (pentagonal-numbers 1))
  ([n] (lazy-seq (cons (pentagonal n) (pentagonal-numbers (inc n))))))

(defn all-pairs [coll]
  (let [[x & xs] coll]
    (when xs
      (lazy-cat
        (map (fn [y] [x y]) xs)
        (all-pairs xs)))))

(defn run []
  (first (drop-while #(let [[p1 p2] %
                            ps (+ p1 p2)
                            pd (- p2 p1)]
                       (not (and (pentagonal? ps) (pentagonal? pd))))
           (all-pairs (take 10000 (pentagonal-numbers))))))