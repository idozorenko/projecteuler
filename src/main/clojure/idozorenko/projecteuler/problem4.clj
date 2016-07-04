(ns idozorenko.projecteuler.problem4)

(letfn [(palindrome? [n]
          (let [s (str n)]
            (= (seq s) (reverse s))))]
  (apply max
    (for [i (range 900 1000)
          j (range i 1000)
          :let [p (* i j)]
          :when (palindrome? p)] p)))
