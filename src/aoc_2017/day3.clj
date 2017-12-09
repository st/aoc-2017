(ns aoc-2017.day3)

(def dirs [:right :up :left :down])
(def dir-map (zipmap dirs [[1 0] [0 1] [-1 0] [0 -1]]))
(def dir-indexes (zipmap dirs (range)))

(defn next-dir
  [dir]
  (-> dir
      dir-indexes
      inc
      (mod (count dirs))
      dirs))

(defn move
  [[x y] dir]
  (let [[dx dy] (dir-map dir)]
    [(+ x dx) (+ y dy)]))

(defn adjacents
  [[x y]]
  (let [r (range -1 2)]
    (into #{}
          (for [dx r dy r :when (not= [0 0] [dx dy])]
            [(+ x dx) (+ y dy)]))))

(defn compute-next-n-adjacent
  [n xys next-xy]
  (->> xys
       (filter (fn [[k v]] ((adjacents next-xy) k)))
       (reduce (fn [sum [k v]]
                 (+ sum v)) 0)))

(defn spiral
  ([compute-next-n-fn] (spiral [1 [0 0]] :down {[0 0] 1} compute-next-n-fn))
  ([[n xy] dir xys compute-next-n-fn]
   (lazy-seq
     (cons [n xy]
           (let [candidate-next-dir (next-dir dir)
                 next-xy-next-dir (move xy candidate-next-dir)
                 keep-same-dir? (xys next-xy-next-dir)

                 next-xy (if keep-same-dir? (move xy dir) next-xy-next-dir)

                 next-n (compute-next-n-fn n xys next-xy)

                 real-next-dir (if keep-same-dir? dir candidate-next-dir)
                 next-xys (assoc xys next-xy next-n)]

             (spiral [next-n next-xy] real-next-dir next-xys compute-next-n-fn))))))

(def spiral-adjacents (partial spiral compute-next-n-adjacent))

(def spiral-numbers (partial spiral (fn[n _ _] (inc n))))

(defn manhattan-distance
  [xy]
  (apply + (map #(Math/abs %) xy)))

(defn sol
  [n]
  (-> (nth (spiral-numbers) (dec n))
      second
      manhattan-distance))

(defn sol2
  [n]
  (some (fn [[i xy]] (when (< n i) i)) (spiral-adjacents)))