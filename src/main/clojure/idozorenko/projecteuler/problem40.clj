(ns idozorenko.projecteuler.problem40)

(apply * (for [max (map #(int (Math/pow 10 %)) (range 7))]
           (loop [l 0
                  i 1]
             (let [s (str i)
                   ll (+ l (count s))]
               (if (>= ll max)
                 (read-string (str (nth s (- max l 1))))
                 (recur ll (inc i)))))))