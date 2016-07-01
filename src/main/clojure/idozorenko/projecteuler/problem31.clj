(ns idozorenko.projecteuler.problem31)

(def coins [1 2 3 5 10 20 50 100 200])

(defn check [fn a b c d e f g h ]
  (fn 200 (+ (* a 200) (* b 100) (* c 50) (* d 20) (* e 10) (* f 5)  (* g 2) (* h 1))))

(defn run []
  (count
   (for [a (range (inc (/ 200 200)))
         :when (check >= a 0 0 0 0 0 0 0)
         b (range (inc (/ 200 100)))
         :when (check >= a b 0 0 0 0 0 0)
         c (range (inc (/ 200 50)))
         :when (check >= a b c 0 0 0 0 0)
         d (range (inc (/ 200 20)))
         :when (check >= a b c d 0 0 0 0)
         e (range (inc (/ 200 10)))
         :when (check >= a b c d e 0 0 0)
         f (range (inc (/ 200 5)))
         :when (check >= a b c d e f 0 0)
         g (range (inc (/ 200 2)))
         :when (check >= a b c d e f g 0)
         h (range (inc (/ 200 1)))
         :when (check = a b c d e f g h)]
     1)))