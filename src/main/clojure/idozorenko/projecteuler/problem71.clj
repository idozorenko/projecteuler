(ns idozorenko.projecteuler.problem71)

(defn solve []
  (let [d 1000000]
   (->> (for [n (range 1 d)

              d (range (- d 10) (inc d))]
          (/ n d))
     (reduce (fn [acc x]
               (if (and (< x 3/7) (> x acc)) x acc)) 0))))
