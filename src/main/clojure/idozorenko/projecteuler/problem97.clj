(ns idozorenko.projecteuler.problem97
  (:require [clojure.string :as str]))

(defn solve []
  (str/join
    (take-last 10
      (str (.add
             (.multiply
               (.shiftLeft (BigInteger. "2") (dec 7830457))
               (BigInteger. "28433"))
             (BigInteger. "1")))))
  )
