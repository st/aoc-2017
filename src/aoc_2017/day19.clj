(ns aoc-2017.day19
  (:require
    [clojure.string :as s]))

(defn cut-lines
  [input]
  (-> input
      slurp
      (s/split #"\n")))

(defn enlarge
  [line width]
  (let [to-add (apply str (repeat (- width (count line)) " "))]
    (str line to-add)))

(defn build-maze
  [input]
  (let [lines (cut-lines input)
        width (apply max (map count lines))
        lines-same-width (map #(enlarge % width) lines)]
    (->> lines-same-width
         vec
         (map vec))))

(defn entry
  [maze]
  [(-> maze
       first
       (.indexOf \|))
   0
   :down])

(def dirs {:down  [0 1]
           :right [1 0]
           :up    [0 -1]
           :left  [-1 0]})

(def verticals #{:down :up})
(def horizontals #{:left :right})

(defn other-dirs
  [dir]
  (if (verticals dir)
    horizontals
    verticals))

(defn in-maze?
  [maze [x y]]
  (and (< y (count maze))
       (< x (count (first maze)))))

(defn no-val?
  [val]
  (or (nil? val)
      (= \space val)))

(defn val-at
  ([maze [x y]]
   (when (in-maze? maze [x y])
     (-> maze
         (nth y)
         (nth x))))
  ([maze [x y] dir]
   (let [[dx dy] (dir dirs)]
     (val-at maze [(+ x dx) (+ y dy)]))))

(defn change-dir?
  [maze [x y]]
  (= \+ (val-at maze [x y])))

(defn next-dir
  [maze [x y] dir]
  (if (change-dir? maze [x y])
    (let [other-dirs (other-dirs dir)
          possibles (remove #(no-val? (val-at maze [x y] %)) other-dirs)]
      (first possibles))
    dir))

(defn finished?
  [maze [x y dir]]
  (and (not (change-dir? maze [x y]))
       (no-val? (val-at maze [x y] dir))))

(defn next-pos
  [maze [x y dir]]
  (let [new-dir (next-dir maze [x y] dir)]
    (let [[dx dy] (new-dir dirs)
          [cx cy] [(+ x dx) (+ y dy)]
          val (val-at maze [cx cy])]
      [cx cy new-dir])))

(defn seen
  [input]
  (loop [maze (build-maze input) pos (entry maze) seen []]
    (let [next-pos (next-pos maze pos)
          next-dir (get next-pos 2)
          new-seen (conj seen (val-at maze pos))]
      (if (finished? maze pos)
        new-seen
        (recur maze next-pos new-seen)))))

(defn sol1
  [input]
  (->> input
       seen
       (filter #(Character/isLetter %))
       (apply str)))

(defn sol2
  [input]
  (->> input
       seen
       count))