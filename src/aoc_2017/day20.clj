(ns aoc-2017.day20
  (:require
    [clojure.string :as s]))

(defn parse-acc
  [line]
  (let [start (.lastIndexOf line "<")
        end (.lastIndexOf line ">")
        raw-acc (subs line (inc start) end)]
    (map read-string (s/split raw-acc #","))))

(defn distance
  [[x y z]]
  (reduce + (map #(Math/abs %) [x y z])))

(defn indexed-lines
  [input]
  (->> (s/split (slurp input) #"\n")
       (map-indexed (fn [i e] [i e]))))

(defn score
  [[i line]]
  (distance (parse-acc line)))

(defn sol
  [input]
  (->> input
       indexed-lines
       (sort-by score)
       ffirst))