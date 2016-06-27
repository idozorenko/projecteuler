(ns projecteuler.problem17
  (:require [clojure.string :as str]))

(def ones-to-word
  {1 "one"
   2 "two"
   3 "three"
   4 "four"
   5 "five"
   6 "six"
   7 "seven"
   8 "eight"
   9 "nine"})

(def teens-to-word
  {1 "eleven"
   2 "twelve"
   3 "thirteen"
   4 "fourteen"
   5 "fifteen"
   6 "sixteen"
   7 "seventeen"
   8 "eighteen"
   9 "nineteen"})

(def tens-to-word
  {1 "ten"
   2 "twenty"
   3 "thirty"
   4 "forty"
   5 "fifty"
   6 "sixty"
   7 "seventy"
   8 "eighty"
   9 "ninety"})

(defn hundreds-to-word [x]
  (when (> x 0)
    (str (get ones-to-word x) " hundred")))

(defn thousands-to-word [x]
  (when (> x 0)
    (str (get ones-to-word x) " thousand")))

(defn to-word [num]
  (let [ones (mod num 10)
        tens (mod (int (/ num 10)) 10)
        hundreds (mod (int (/ num 100)) 10)
        thousands (mod (int (/ num 1000)) 1000)]
    (->> [(thousands-to-word thousands)
          (hundreds-to-word hundreds)
          (if (and (>= ones 1) (= tens 1))
            (teens-to-word ones)
            (when (or (> ones 0) (> tens 0))
              (str
               (tens-to-word tens)
               " "
               (ones-to-word ones))))]
      (filter identity)
      (str/join " and "))))

(defn count-symbols [s]
  (count (str/replace s " " "")))

