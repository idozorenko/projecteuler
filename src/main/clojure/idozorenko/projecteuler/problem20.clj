(ns idozorenko.projecteuler.problem20)

(defn factorial [n]
  (apply * (map bigint (range 1 (inc n)))))

(defn digit-sum [n]
  (reduce #(+ (read-string (str %2)) %1) 0 (str n)))

#_(factorial (digit-sum 100))