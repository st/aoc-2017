(ns aoc-2017.day18
  (:require
    [clojure.string :as s]))

(defmulti command (fn [_ op _ _] op))

(defn register-value
  [m x]
  (let [v (read-string x)]
    (if (number? v)
      v
      (m x 0))))

(defn exec-op
  [m f a b]
  (assoc m a (f (register-value m a)
                (register-value m b))))

(defmethod command "set"
  [m _ a b]
  (assoc m a (register-value m b)))

(defmethod command "mul"
  [m _ a b]
  (exec-op m * a b))

(defmethod command "add"
  [m _ a b]
  (exec-op m + a b))

(defmethod command "mod"
  [m _ a b]
  (exec-op m mod a b))

(defmethod command "snd"
  [m _ a b]
  (assoc m :snd (register-value m a)))

(defmethod command "rcv"
  [m _ a b]
  (if (< 0 (register-value m a))
    (assoc m :rcv (:snd m))
    m))

(defmethod command :default
  [m op a b]
  m)

(defmulti move-cpt (fn [_ op _ _] op))

(defmethod move-cpt "jgz"
  [m _ a b]
  (if (< 0 (register-value m a))
    (update m :cpt #(+ % (register-value m b)))
    (update m :cpt inc)))

(defmethod move-cpt :default
  [m _ a b]
  (update m :cpt inc))

(defn execute-line
  [line memory]
  (let [[cmd a b] (s/split line #" ")]
    (-> memory
        (command cmd a b)
        (move-cpt cmd a b))))

(defn finished?
  [m]
  (< 0 (get m :rcv 0)))

(defn execute-program
  ([lines]
   (execute-program lines {:cpt 0}))
  ([lines memory]
   (let [line (nth lines (:cpt memory))
         new-m (execute-line line memory)]
     (if (finished? new-m)
       new-m
       (recur lines new-m)))))

(defn sol
  []
  (-> "src/resources/day18.txt"
      slurp
      (s/split #"\n")
      execute-program
      :rcv))