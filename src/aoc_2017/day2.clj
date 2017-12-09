(ns aoc-2017.day2)

(defn cs-line
  [line]
  (- (apply max line) (apply min line)))

(defn cs
  [lines]
  (reduce #(+ %1 (cs-line %2)) 0 lines))