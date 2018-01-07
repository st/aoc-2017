(ns aoc-2017.day15)

(defn last-16-bytes
  [l]
  (mod l 0x10000))

(defn matches?
  [[a b]]
  (= (last-16-bytes a) (last-16-bytes b)))

(defn values
  [start multiplier p]
  (filter p (iterate #(-> % (* multiplier) (mod 2147483647) long) start)))

(defn pair-values
  [s1 m1 p1 s2 m2 p2]
  (map vector (values s1 m1 p1) (values s2 m2 p2)))

(defn nb-matches
  [s1 m1 p1 s2 m2 p2 n]
  (->> (pair-values s1 m1 p1 s2 m2 p2)
       (take (inc n))
       (filter matches?)
       count))

(defn always-true
  [_]
  true)

(defn sol1
  [s1 m1 s2 m2 n]
  (nb-matches s1 m1 always-true s2 m2 always-true n))

(defn mk-multiple-of?
  [n]
  (fn [x]
    (zero? (mod x n))))

(defn sol2
  [s1 m1 s2 m2 n]
  (nb-matches s1 m1 (mk-multiple-of? 4)
              s2 m2 (mk-multiple-of? 8) n))
