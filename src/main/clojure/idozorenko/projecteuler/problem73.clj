(ns idozorenko.projecteuler.problem73)

(defn farey
  ([a b c n p]
   (loop [a a
          b b
          c c
          d (dec n)
          f 0]
     (if (p a b c d)
       f
       (let [k (int (/ (+ n b) d))
             p (- (* k c) a)
             q (- (* k d) b)
             f (inc f)]
         (recur c d p q f))))))

(defn solve []
  (farey 1 3 4000 12000
    #(and (= %3 1) (= %4 2))))
