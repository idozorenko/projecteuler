(ns idozorenko.projecteuler.problem63)

(defn solve []
  (->> (range 1 10)
    (mapcat
      (fn [i]
        (let [x (take-while (fn [pow]
                              (let [r (BigDecimal. (Math/pow i pow))]
                                (= pow (count (str r))))) (iterate inc 1))]
          (->> x
            (map #(BigDecimal. (Math/pow i %)))))))
    (set)
    (count)))