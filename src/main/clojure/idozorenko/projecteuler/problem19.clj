(ns idozorenko.projecteuler.problem19)

(def days-in-month
  {1 31
   2 28
   3 31
   4 30
   5 31
   6 30
   7 31
   8 31
   9 30
   10 31
   11 30
   12 31})

(defn get-month-length [m y]
  (if (and (= 2 m)
           (and (zero? (mod y 4))
                (or (not (zero? (mod y 100)))
                    (zero? (mod y 400)))))
    29
    (get days-in-month m)))

(defn add-day [[d m y]]
  (let [d (inc d)]
    (if (> d (get-month-length m y))
      (let [d 1
            m (inc m)]
        (if (> m 12)
          (let [m 1
                y (inc y)]
            [d m y])
          [d m y]))
      [d m y])))

(defn sunday? [days-since-1900]
  (zero? (mod (inc days-since-1900) 7)))

(defn get-count-first-day-of-month-sunday []
  (loop [d 1
         m 1
         y 1900
         days-since-1900 0
         cnt 0]
    (if (> y 2000)
      cnt
      (let [cnt (if (and (= d 1)
                         (> y 1900)
                         (sunday? days-since-1900))
                  (inc cnt)
                  cnt)
            [d m y] (add-day [d m y])]
        (recur d m y (inc days-since-1900) cnt)))))