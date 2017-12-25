(ns aoc-2017.day11)

(def half-sqrt3 (/ (Math/sqrt 3.0) 2.0))

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

(defn toward-origin
  [[x y]]
  (->> dir-deltas
       vals
       (map (partial move [x y]))
       (map (fn [[x y]] [[x y] (compute-distance [x y])]))
       (sort-by second)
       ffirst))

(def mem-toward-origin (memoize toward-origin))

(defn move-by-dirs
  [x-y dirs]
  (->> dirs
       (map dir-deltas)
       (reduce move x-y)))

(defn origin?
  [[x y]]
  (<= (compute-distance [x y]) 1e-10))

(defn nb-steps
  [dirs]
  (->> dirs
       (move-by-dirs [0 0])
       (iterate mem-toward-origin)
       (take-while (complement origin?))
       count))

(def mem-nb-steps (memoize nb-steps))

(defn successive
  [xs]
  (reduce #(conj %1 (vec (conj (last %1) %2))) [] xs))

(defn nb-steps-s
  [s]
  (->> (clojure.string/split s #",")
       nb-steps))

(defn furthest
  [s]
  (->> (clojure.string/split s #",")
       successive
       (map nb-steps)
       (apply max)))

(defn mem-furthest
  [s]
  (->> (clojure.string/split s #",")
       successive
       (map mem-nb-steps)
       (apply max)))

(defn sol1
  []
  (nb-steps-s (slurp "src/resources/day11.txt")))

(defn sol2
  []
  (time (furthest (slurp "src/resources/day11.txt"))))

(defn sol3
  []
  (time (mem-furthest (slurp "src/resources/day11.txt"))))