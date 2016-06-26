(ns idozorenko.projecteuler.problem28)


(defn diagonal-sum [n]
  (loop [lev 1
         sum 1
         last-num 1]
    (if (> lev n)
      sum
      (let [nums (for [i (range 1 5)] (+ last-num (* i lev 2)))
            sum (+ sum (reduce + nums))]
        (recur (inc lev) sum (last nums))))))