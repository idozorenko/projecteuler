(ns idozorenko.projecteuler.problem6)

(let [r (range 1 101)]
  (-
    (let [sum (reduce + r)] (* sum sum))
    (->> r (map #(* % %)) (reduce +))))