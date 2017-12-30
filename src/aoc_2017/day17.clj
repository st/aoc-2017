(ns aoc-2017.day17)

(defn spin
  [size [coll cursor value]]
  (let [insertion-index (inc (mod (+ cursor size) (count coll)))
        new-coll (into [] (concat (subvec coll 0 insertion-index)
                                  [value]
                                  (subvec coll insertion-index)))
        new-v (inc value)]
    [new-coll insertion-index new-v]))

(defn short-circuit
  [size]
  (let [[coll index _]
        (nth (iterate (partial spin size) [[0] 0 1]) 2017)]
    (nth coll (mod (inc index) (count coll)))))