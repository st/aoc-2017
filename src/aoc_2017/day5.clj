(ns aoc-2017.day5)

(defn next-cursor-offsets
  [cursor-offsets cursor-fn]
  (let [cursor    (first cursor-offsets)
        offsets   (second cursor-offsets)
        nb-steps  (nth offsets cursor)]
    (-> cursor-offsets
        (update 0 #(+ % nb-steps))
        (update-in [1 cursor] cursor-fn))))

(defn next-until-out
  ([cursor-offsets cursor-fn]
   (next-until-out 0 cursor-offsets cursor-fn))
   ([gen cursor-offsets cursor-fn]
    (if (>= (first cursor-offsets) (count (second cursor-offsets)))
      gen
      (recur (inc gen) (next-cursor-offsets cursor-offsets cursor-fn) cursor-fn))))

(defn sol1
  [cursor-offsets]
  (next-until-out cursor-offsets inc))

(defn sol2
  [cursor-offsets]
  (next-until-out cursor-offsets (fn [c] (if (>= c 3)
                                           (dec c)
                                           (inc c)))))

