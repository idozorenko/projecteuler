(ns idozorenko.projecteuler.problem65)

(defn solve []
  (let [n 100
        [conv & rest] (reverse
                        (cons 2
                          (take (dec n)
                            (flatten
                              (for [i (range 1 (inc (/ n 3)))]
                                [1 (* 2 i) 1])))))
        nth-convergent (loop [k 1
                              fr conv
                              [conv & rest] rest]
                         (let [fr (+ conv (/ 1 fr))]
                           (if (= k (dec n))
                             fr
                             (recur (inc k) fr rest))))]
    (->> nth-convergent
      (numerator)
      (str)
      (map #(Character/digit % 10))
      (reduce +))))