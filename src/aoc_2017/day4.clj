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

(defn pairs
  [coll]
  (loop [[x & xs] coll res []]
    (if (empty? xs)
      res
      (recur xs (concat res (map (fn[e] [x e]) xs))))))

(defn no-anagram?
  [s]
  (->> s words pairs (not-any? anagram?)))

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