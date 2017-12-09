(ns aoc-2017.day1)

(defn pairs-offset
  [coll p]
  (let [n (count coll)]
    (map (fn[i] [(nth coll i)
                 (nth coll (mod (+ i p) n))])
         (range n))))

(defn keep-same
  [coll-pairs]
  (filter (fn [[a b]] (= a b)) coll-pairs))

(defn first-digits
  [coll-pairs]
  (map (fn[[a b]] (-> a str read-string)) coll-pairs))

(defn sol
  [s offset]
  (->> (pairs-offset s offset)
       keep-same
       first-digits
       (reduce +)))

(defn sol1
  [s]
  (sol s 1))

(defn sol2
  [s]
  (sol s (/ (count s) 2)))

