(ns aoc-2017.day2)

(defn cs-line
  [line]
  (- (apply max line) (apply min line)))

(defn cs-divide
  [[a b]]
  (when (zero? (mod b a))
    (/ b a)))

(defn cs-divide-line
  [line]
  (let [r (range (count line))
        pairs (for [i r j r :when (not= i j)]
                (sort [(nth line i) (nth line j)]))]
    (some cs-divide pairs)))

(defn cs
  [f lines]
  (reduce #(+ %1 (f %2)) 0 lines))

(defn sol1
  [lines]
  (cs cs-line lines))

(defn sol2
  [lines]
  (cs cs-divide-line lines))