(ns idozorenko.projecteuler.problem5)

(defn get-prime-divisors [num]
  (loop [[dv & dvsrest :as dvs] (range 2 (inc num))
         mults []
         num num]
    (if (= 1 num)
      mults
      (let [[dvs mults num] (if (zero? (mod num dv))
                              [dvs (cons dv mults) (/ num dv)]
                              [dvsrest mults num])]
       (recur dvs mults num)))))

(reduce *
  (flatten
    (vals
      (apply merge-with
        (cons #(if (< (count %1) (count %2)) %2 %1)
          (for [i (range 2 1000)]
            (->> (get-prime-divisors i)
              (map bigint)
              (group-by identity))))))))