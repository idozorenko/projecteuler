(ns idozorenko.projecteuler.problem30)

(def powers
  (into {} (for [i (range 10)]
             [(keyword (str i)) (int (Math/pow i 5))])))

(let [test? #(let [s (str %)]
              (= % (reduce + (for [c s]
                               (get powers (keyword (str c)))))))]
  (reduce
    #(cond-> % (test? %2) (+ %2))
    (range 1 (inc 1E6))))