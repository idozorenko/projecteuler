(ns idozorenko.projecteuler.problem39)

(defn run []
  (let [p 1000]
   (reduce
     #(max (second %2) (second %))
     [-1 0]
     (for [p (range 3 (inc p))]
       [p (count
            (->> (for [a (range 1 (inc p))
                       b (range 1 (inc p))
                       :let [c (Math/sqrt (+ (* b b) (* a a)))]
                       :when (= c (Math/floor c))
                       :when (and (= p (+ a b (int c))))]
                   [a b (int c)])
              (map sort)
              (distinct)))]))))