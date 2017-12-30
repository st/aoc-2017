(ns aoc-2017.day16
  (:require
    [clojure.string :as s]))

(defmulti dance (fn [_ op] (first op)))

(defmethod dance \s
  [s op]
  (let [len (count s)
        n (-> op (subs 1) read-string (mod len))
        first-part (subs s 0 (- len n))
        second-part (subs s (- len n) len)]
    (str second-part first-part)))

(defn replace-at
  [s n c]
  (apply str (assoc (vec s) n c)))

(defmethod dance \x
  [s op]
  (let [[a b] (map read-string (-> op (subs 1) (s/split #"/")))
        char-at-a (nth s a)
        char-at-b (nth s b)]
    (-> s
        (replace-at a char-at-b)
        (replace-at b char-at-a))))

(defmethod dance \p
  [s op]
  (let [[a b] (map first (-> op (subs 1) (s/split #"/")))]
    (-> s
        (s/replace a \.)
        (s/replace b a)
        (s/replace \. b))))

(defn repeat-f
  [f x n]
  (nth (iterate f x) n))

(defn all-dances
  ([steps s]
   (reduce dance s steps))
  ([steps s n]
   (repeat-f (partial all-dances steps) s n)))

(defn idempotent
  ([f x]
   (idempotent f x 1 (f x)))
  ([f x n v]
   (if (= x v)
     n
     (recur f x (inc n) (f v)))))

(defn sol
  [n]
  (let [steps (s/split (slurp "src/resources/day16.txt") #",")
        f (partial all-dances steps)
        input "abcdefghijklmnop"
        n (if (= 1 n)
            1
            (mod n (idempotent f input)))]
    (all-dances steps input n)))

