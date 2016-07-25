(ns idozorenko.projecteuler.problem53)

(defn factorial [n]
  (apply * (map bigint (range 1 (inc n)))))

(defn combinations [n r]
  (/ (factorial n) (* (factorial r) (factorial (- n r)))))

(reduce +
  (for [i (range 1 101)]
    (count
      (->> (range 1 i)
        (map #(combinations i %))
        (filter #(< 1E6 %))))))