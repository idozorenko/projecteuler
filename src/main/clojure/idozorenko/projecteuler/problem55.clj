(ns idozorenko.projecteuler.problem55)

(defn reverse-number
  ([number] (reverse-number number (bigint 0)))
  ([number reversed] (if (= number 0)
                       reversed
                       (reverse-number (quot number 10)
                         (+ (rem number 10) (* reversed 10))))))

(defn palindromic? [n]
  (= n (reverse-number n)))

(defn lychrel? [n]
  (loop [i 50
         n n]
    (let [x (+ n (reverse-number n))]
      (if (palindromic? x)
        false
        (if (zero? i)
          true
          (recur (dec i) x))))))

(->> (range 1 10000)
  (filter lychrel?)
  (count))