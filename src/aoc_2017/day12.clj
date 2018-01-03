(ns aoc-2017.day12
  (:require
    [clojure.set :as set]
    [clojure.string :as s]))

(defn line->set
  [line]
  (->> (s/split line #"<->")
       (map s/trim)
       (map #(s/split % #","))
       flatten
       (map s/trim)
       (into #{})))

(defn intersect?
  [s1 s2]
  (not (empty? (set/intersection s1 s2))))

(defn glue
  ([sets s]
   (glue sets s false []))
  ([[current & remain] s success res]
   (if (not current)
     (if (not success)
       (conj res s)
       res)
     (if (intersect? s current)
       (let [glued (set/union s current)]
         (recur remain s true (glue res glued)))
       (recur remain s success (conj res current))))))

(defn groups
  [input]
  (->> (s/split (slurp input) #"\n")
       (reduce #(glue %1 (line->set %2)) [])))

(defn sol
  [input]
  (->> input
       groups
       (some (fn [s] (if (s "0") s)))
       count))

(defn sol2
  [input]
  (->> input
       groups
       count))