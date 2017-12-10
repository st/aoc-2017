(ns aoc-2017.day4
  (:require
    [clojure.java.io :as io]
    [clojure.string :refer [split]]))

(defn pp-lines
  []
  (-> "resources/passphrases.txt"
      io/resource
      slurp
      (split #"\n")))

(defn words
  [s]
  (split s #" "))

(defn no-repeat?
  [s]
  (->> s
       words
       (apply distinct?)))

(defn anagram?
  [[s1 s2]]
  (= (sort s1) (sort s2)))

(defn no-anagram?
  [s]
  (let [ws (words s)
        r (range (count ws))
        pairs (for [i r j r :when (not= i j)]
                [(nth ws i) (nth ws j)])]
    (not-any? anagram? pairs)))

(defn nb-for-predicate
  [p coll]
  (->> coll
       (filter p)
       count))

(defn sol
  []
  (nb-for-predicate no-repeat? (pp-lines)))

(defn sol2
  []
  (nb-for-predicate no-anagram? (pp-lines)))