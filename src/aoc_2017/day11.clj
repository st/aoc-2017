(ns aoc-2017.day11)

(def sqrt3 (Math/sqrt 3.0))
(def half-sqrt3 (/ sqrt3 2.0))

(def dir-deltas {"n"  [0 1]
                 "nw" [(- half-sqrt3) 0.5]
                 "sw" [(- half-sqrt3) -0.5]
                 "s"  [0 -1]
                 "se" [half-sqrt3 -0.5]
                 "ne" [half-sqrt3 0.5]})

(defn compute-distance
  [[x y]]
  (Math/sqrt (+ (* x x) (* y y))))

(defn move
  [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn next-to-origin
  [[x y]]
  (->> dir-deltas
       vals
       (map (partial move [x y]))
       (map (fn [[x y]] [[x y] (compute-distance [x y])]))
       (sort-by second)
       ffirst))

;; (apply min (map compute-distance
;;                 (map (partial move [x y]) (vals dir-deltas)))))

(defn move-by-dirs
  [x-y dirs]
  (->> dirs
       (map dir-deltas)
       (reduce move x-y)))

(defn move-s
  [s]
  (->> (clojure.string/split s #",")
       (move-by-dirs [0 0])))

(defn origin?
  [[x y]]
  (<= (compute-distance [x y]) 1e-10))

(defn nb-steps
  [s]
  (->> s
       move-s
       (iterate next-to-origin)
       (take-while (complement origin?))
       count))

(defn sol1
  []
  (nb-steps (slurp "src/resources/day11.txt")))

(def check
  (map nb-steps
       ["ne,ne,ne"
        "ne,ne,sw,sw"
        "ne,ne,s,s"
        "se,sw,se,sw,sw"]))