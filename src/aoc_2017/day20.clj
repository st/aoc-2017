(ns aoc-2017.day20
  (:require
    [clojure.string :as s]
    [clojure.set :as set]))

(defn before
  [s c]
  (let [end (.lastIndexOf s c)]
    (subs s 0 end)))

(defn parse-numbers
  [s]
  (map read-string (s/split (before s ">") #",")))

(defn parse-particule
  [line]
  (let [[_ p v a] (s/split line #"<")]
    (map parse-numbers [p v a])))

(defn parse-acc
  [line]
  (let [[_ _ a] (parse-particule line)]
    a))

(defn add-coordinates
  [as bs]
  (map + as bs))

(defn sub-coordinates
  [as bs]
  (map - as bs))

(defn distance
  ([p]
   (reduce + (map #(* % %) p)))
  ([p0 p1]
   (distance (sub-coordinates p0 p1))))

(defn indexed-lines
  [input]
  (->> (s/split (slurp input) #"\n")
       (map-indexed (fn [i e] [i e]))))

(defn distance-to-origin
  [[i line]]
  (distance (parse-acc line)))

(defn sol
  [input]
  (->> input
       indexed-lines
       (sort-by distance-to-origin)
       ffirst))

(defn next-pos
  [[p v a]]
  (let [new-v (add-coordinates v a)]
    [(add-coordinates p new-v) new-v a]))

(defn remove-collisions
  [ps]
  (->> ps
       (group-by first)
       (filter (fn [[pos ps]] (= 1 (count ps))))
       vals
       (map first)))

(defn remove-collisions-next
  [ps]
  (->> ps
       remove-collisions
       (map next-pos)))

(defn sol2
  ;; beurk
  [input]
  (->> input
       indexed-lines
       (map (fn [[_ l]] (parse-particule l)))
       (iterate remove-collisions-next)))

