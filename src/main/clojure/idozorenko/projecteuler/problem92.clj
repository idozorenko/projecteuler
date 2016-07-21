(ns idozorenko.projecteuler.problem92)

(defn solve []
  (time
    (count
      (filter identity
        (for [i (range 1 1E7)]
          (do
            (loop [i i]
              (let [x (->> (str i)
                        (map #(Integer. (str %)))
                        (map #(* % %))
                        (reduce +))]
                (if (or (= x 89))
                  true
                  (if (= x 1)
                    false
                    (recur x)))))))))))