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

(defn spiral-numbers
  ([] (spiral-numbers [0 0] 1 :down {[0 0] :seen}))
  ([xy n dir xys]
   (lazy-seq
     (cons xy
           (let [candidate-next-dir (next-dir dir)
                 next-xy-next-dir (move xy candidate-next-dir)
                 keep-same-dir? (xys next-xy-next-dir)

                 next-n (inc n)
                 next-xy (if keep-same-dir? (move xy dir) next-xy-next-dir)
                 real-next-dir (if keep-same-dir? dir candidate-next-dir)
                 next-xys (assoc xys next-xy :seen)]
             (spiral-numbers next-xy next-n real-next-dir next-xys))))))

(defn adjacents
  [[x y]]
  (let [r (range -1 2)]
    (into #{}
          (for [dx r dy r :when (not= [0 0] [dx dy])]
            [(+ x dx) (+ y dy)]))))

(defn spiral-adjacents
  ([] (spiral-adjacents [1 [0 0]] :down {[0 0] 1}))
  ([[n xy] dir xys]
   (lazy-seq
     (cons [n xy]
           (let [candidate-next-dir (next-dir dir)
                 next-xy-next-dir (move xy candidate-next-dir)
                 keep-same-dir? (xys next-xy-next-dir)

                 next-xy (if keep-same-dir? (move xy dir) next-xy-next-dir)
                 next-n (->> xys
                             (filter (fn [[k v]] ((adjacents next-xy) k)))
                             (reduce (fn [sum [k v]]
                                       (+ sum v)) 0))
                 real-next-dir (if keep-same-dir? dir candidate-next-dir)
                 next-xys (assoc xys next-xy next-n)]

             (spiral-adjacents [next-n next-xy] real-next-dir next-xys))))))

(defn manhattan-distance
  [xy]
  (apply + (map #(Math/abs %) xy)))

(defn sol
  [n]
  (-> (nth (spiral-numbers) (dec n))
      manhattan-distance))

(defn sol2
  [n]
  (some (fn [[i xy]] (when (< n i) i)) (spiral-adjacents)))