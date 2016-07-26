(ns idozorenko.projecteuler.einstein
  (:require [clojure.math.combinatorics :as combo]))

(def data
  {:nationality [:englishman :swede :dane :german]
  :color [:red :white :yellow :green]
  :pet [:dogs :birds :cats :horses :fish]
  :cigarettes [:pall_mall :dunhills :blend :blue_masters :prince]
  :drink [:tea :coffee :bier :water]})

(defn find-first-index
  [pred coll]
  (first (keep-indexed (fn [idx x]
                   (when (pred x)
                     idx))
     coll)))

(defn group-into-houses [[[n1 n2 n3 n4 n5]
                          [c1 c2 c3 c4 c5]
                          [p1 p2 p3 p4 p5]
                          [g1 g2 g3 g4 g5]
                          [d1 d2 d3 d4 d5]]]
  [{:nationality n1
    :color c1
    :pet p1
    :cigarettes g1
    :drink d1}
   {:nationality n2
    :color c2
    :pet p2
    :cigarettes g2
    :drink d2}
   {:nationality n3
    :color c3
    :pet p3
    :cigarettes g3
    :drink d3}
   {:nationality n4
    :color c4
    :pet p4
    :cigarettes g4
    :drink d4}
   {:nationality n5
    :color c5
    :pet p5
    :cigarettes g5
    :drink d5}])

(defn meets-requirements? [data]
  (let [[h1 h2 h3 h4 h5 :as houses] (group-into-houses data)]
    (and
      (some (fn [{:keys [color nationality]}]
              (and (= :red color) (= :englishman nationality)))
        houses)
      (some (fn [{:keys [pet nationality]}]
              (and (= :dogs pet) (= :swede nationality)))
        houses)
      (some (fn [{:keys [drink nationality]}]
              (and (= :tea drink) (= :dane nationality)))
        houses)
      (= (inc (find-first-index #(= :green (:color %)) houses))
        (find-first-index #(= :white (:color %)) houses))
      (some (fn [{:keys [drink color]}]
              (and (= :coffee drink) (= :green color)))
        houses)
      (some (fn [{:keys [pet cigarettes]}]
              (and (= :birds pet) (= :pall_mall cigarettes)))
        houses)
      (some (fn [{:keys [color cigarettes]}]
              (and (= :yellow color) (= :dunhills cigarettes)))
        houses)
      (= :milk (:drink h3))
      (= :norwegian (:nationality h1))
      (= 1 (Math/abs
             (- (find-first-index #(= :blend (:cigarettes %)) houses)
               (find-first-index #(= :cats (:pet %)) houses))))
      (some (fn [{:keys [drink cigarettes]}]
              (and (= :bier drink) (= :blue_masters cigarettes)))
        houses)
      (= 1 (Math/abs
             (- (find-first-index #(= :dunhills (:cigarettes %)) houses)
               (find-first-index #(= :horses (:pet %)) houses))))
      (some (fn [{:keys [nationality cigarettes]}]
              (and (= :german nationality) (= :prince cigarettes)))
        houses)
      (= 1 (Math/abs
             (- (find-first-index #(= :blue (:color %)) houses)
               (find-first-index #(= :norwegian (:nationality %)) houses))))
      (= 1 (Math/abs
             (- (find-first-index #(= :blend (:cigarettes %)) houses)
               (find-first-index #(= :water (:drink %)) houses)))))))

(defn generator []
  (combo/cartesian-product
    (map
      #(cons :norwegian %)
      (combo/permutations (:nationality data)))
    (map
      #(let [[s e] (split-at 1 %)]
        (concat s [:blue] e))
      (combo/permutations (:color data)))
    (combo/permutations (:pet data))
    (combo/permutations (:cigarettes data))
    (map
      #(let [[s e] (split-at 2 %)]
        (concat s [:milk] e))
      (combo/permutations (:drink data)))))

(defn solve []
  (let [cnt (atom 0)]
    (->> (generator)
      (drop-while #(do
                    (when (zero? (mod (swap! cnt inc) 1E5))
                      (println "Processed " @cnt))
                    (not (meets-requirements? %))))
      (first))))

'((:norwegian :yellow :cats :dunhills :water)
   (:dane :blue :horses :blend :tea)
   (:englishman :red :birds :pall_mall :milk)
   (:german :green :fish :prince :coffee)
   (:swede :white :dogs :blue_masters :bier))