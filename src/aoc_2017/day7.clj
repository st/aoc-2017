(ns aoc-2017.day7
  (:require
    [clojure.string :as s]
    [clojure.java.io :as io]
    [clojure.zip :as z]))

(defn ->children
  [s]
  (when s
    (->> (s/split s #",")
         (map s/trim)
         (map (fn [n] {n nil}))
         vec)))

(defn ->node
  [parent]
  (let [[_ name weight] (first (re-seq #"(\w+) \((\d+)\)" parent))]
    {name (read-string weight)}))

(defn line->tree
  [line]
  (let [[parent children] (s/split line #"->")]
    [(->node parent) (->children children)]))

(defn program-lines
  []
  (-> "resources/programs.txt"
      io/resource
      slurp
      (s/split #"\n")))

(defn same-key?
  [m1 m2]
  (println "m1" m1)
  (println "m2" m2)
  (= (first (keys (first m1))) (first (keys m2))))

(defn weight-child
  [t c]
  (loop [loc (z/down t)]
    (if (z/end? loc)
      c
      (if (and (not (z/branch? loc))
               (same-key? (z/node loc) c))
        (z/node loc)
        (recur (z/next loc))))))

(defn children-weighted
  [t cs]
  (map (partial weight-child t) cs))

(defn insert-parent-children
  [t p cs]
  (let [children (children-weighted t cs)]
    (-> t
        (z/insert-child (concat [p] children)))))

(defn build-tree
  []
  (reduce
    (fn [tree [p cs]]
      (insert-parent-children tree p cs))
    (z/vector-zip [])
    (map line->tree (program-lines))))