(ns idozorenko.projecteuler.problem62)

(defn solve []
  (-> (loop [r {}
             i (BigInteger. "1")]
        (let [cube (* i i i)
              s (sort (str cube))
              [v nums] (r s)
              v (inc (or v 0))
              nums (conj (or nums []) cube)]
          (if (= 5 v)
            nums
            (recur (assoc r s [v nums]) (.add i (BigInteger. "1"))))))
    (sort)
    (first)))