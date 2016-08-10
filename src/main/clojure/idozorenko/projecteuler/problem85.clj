(ns idozorenko.projecteuler.problem85)


(defn get-rectangles2 [i j]
  (reduce +
    (for [x (map inc (range i))
          y (map inc (range j))]
      (* (inc (- j y)) (inc (- i x))))))

(defn get-rectangles [i j]
  (/ (* i (inc i) j (inc j)) 4))

(defn delta [rect]
  (Math/abs (- 2E6 rect)))

(defn solve []
  (let [max-i (first (drop-while #(> 2E6 (get-rectangles % 1)) (iterate inc 1)))]
    (reduce
      #(if (< (delta (:rect %)) (delta (:rect %2))) % %2)
      (for [i (range 1 max-i)
            j (range 1 i)]
       {:rect (get-rectangles i j)
        :area (* i j)
        :w i :h j}))))