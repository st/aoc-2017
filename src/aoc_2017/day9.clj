(ns aoc-2017.day9)

(defn close-group
  [levels level]
  (if (levels level)
    (update levels level inc)
    (assoc levels level 1)))

(defn read-chars
  ([cs]
   (read-chars {} 0 false cs))
  ([levels level garbage? cs]
   (let [c (first cs)
         rest-cs (rest cs)]
     (cond
       (empty? cs) levels

       (= \! c) (recur levels level garbage? (rest rest-cs))
       (= \> c) (recur levels level false rest-cs)
       garbage? (recur levels level garbage? rest-cs)

       (= \< c) (recur levels level true rest-cs)
       (= \{ c) (recur levels (inc level) garbage? rest-cs)
       (= \} c) (recur (close-group levels level) (dec level) garbage? rest-cs)

       :else (recur levels level garbage? rest-cs)))))

(defn weight-groups
  [groups]
  (->> groups
       (map (fn [[k v]] (* k v)))
       (reduce +)))

(defn sol
  [s]
  (weight-groups (read-chars s)))

(defn count-garbage
  ([s]
   (count-garbage 0 false s))
  ([res garbage? s]
   (let [c (first s)
         rest-cs (rest s)]
     (cond
       (empty? s) res
       (= \! c)   (recur res garbage? (rest rest-cs))
       (= \> c)   (recur res false rest-cs)
       garbage?   (recur (inc res) garbage? rest-cs)
       (= \< c)   (recur res true rest-cs)
       :else      (recur res garbage? rest-cs)))))
