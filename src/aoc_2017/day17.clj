(ns aoc-2017.day17)

(defn spin
  [[coll cursor value] size nb-spins]
  (if (zero? nb-spins)
    [coll cursor value]
    (let [insertion-index (inc (mod (+ cursor size) value))
          new-coll (into [] (concat (subvec coll 0 insertion-index)
                                    [value]
                                    (subvec coll insertion-index)))
          new-v (inc value)]
      (recur [new-coll insertion-index new-v] size (dec nb-spins)))))

(defn spin-simple
  [size nb-spins]
  (loop [res 0 cursor 0 value 1 size size nb-spins nb-spins]
    (if (zero? nb-spins)
      res
      (let [insertion-index (inc (mod (+ cursor size) value))
            new-v (inc value)
            new-res (if (= 1 insertion-index) value res)]
        (recur new-res insertion-index new-v size (dec nb-spins))))))

(defn short-circuit
  [nb-steps nb-spins]
  (let [[coll index _]
        (spin [[0] 0 1] nb-steps nb-spins)]
    (nth coll (inc index))))