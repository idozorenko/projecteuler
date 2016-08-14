(ns idozorenko.projecteuler.problem76)


(def partitions
  (memoize
    (fn [n m]
      (if (or (zero? m) (< n 0))
        0
        (if (or (zero? n) (= m 1))
          1
          (+
            (partitions n (dec m))
            (partitions (- n m) m)))))))

(defn solve []
  (partitions 100 99))