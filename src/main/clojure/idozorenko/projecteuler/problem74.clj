(ns idozorenko.projecteuler.problem74)

(def chain-length-cache (atom {}))

(defn factorial [n]
  (loop [i 1
         f 1]
    (if (> i n) f
                (recur (inc i) (* f i)))))

(defn cache-chain [terms total-length]
  (dorun
    (map-indexed
      (fn [i term]
        (swap! chain-length-cache assoc term (- total-length i)))
      terms)))

(defn digits [n]
  (->> n
    (iterate #(quot % 10))
    (take-while pos?)
    (mapv #(mod % 10))
    rseq))

(defn get-number-factorial-sum [n]
  (->> n
    (digits)
    (map #(factorial %))
    (reduce +)))

(def CHAIN_LENGTH 60)

(defn test-term-count [n]
  (loop [terms [n]
         current n]
    (let [cnt (count terms)]
      (if (> cnt CHAIN_LENGTH)
        false
        (if-let [current-length (@chain-length-cache current)]
          (let [cnt (+ (count terms) (dec current-length))]
            (cache-chain terms cnt)
            (= cnt CHAIN_LENGTH))
          (let [new (get-number-factorial-sum current)]
           (if (some #{new} terms)
             (do
               (cache-chain terms cnt)
               (= cnt CHAIN_LENGTH))
             (recur (conj terms new) new))))))))

(defn solve []
  (time
   (let [n (atom 0)]
     (doseq [i (range 1000000)]
       (when (test-term-count i)
         (swap! n inc)))
     @n)))