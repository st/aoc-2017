(ns aoc-2017.day8
  (:require
    [clojure.string :as s]
    [clojure.java.io :as io]))

(defn ->val
  [s registers]
  (let [x (read-string s)]
    (if (number? x)
      x
      (get registers (keyword x) 0))))

(defn ->f
  [s]
  (-> s
      (s/replace "!=" "not=")
      symbol
      resolve))

(defn ->f-modif
  [s]
  (cond
    (= s "inc") +
    (= s "dec") -
    :else (throw (Exception. "error"))))

(defn eval-cond
  [op1 op op2 registers]
  (let [f     (->f op)
        val1  (->val op1 registers)
        val2  (->val op2 registers)]
    (f val1 val2)))

(defn process-line
  [[max-val registers] line]
  (let [[target modif increment _ op1 op op2] (s/split line #" ")
        target-k                              (-> target read-string keyword)
        current-val                           (registers target-k 0)
        f-modif                               (->f-modif modif)
        increment-value                       (->val increment registers)
        do-modif?                             (eval-cond op1 op op2 registers)
        new-registers                         (if do-modif?
                                                (-> registers
                                                    (assoc target-k current-val)
                                                    (update target-k f-modif increment-value))
                                                (assoc registers target-k current-val))]
    [(max max-val (new-registers target-k)) new-registers]))

(defn registers-lines
  []
  (-> "resources/registers.txt"
      io/resource
      slurp
      (s/split #"\n")))

(defn registers
  []
  (reduce process-line [0 {}] (registers-lines)))

(defn sol1
  []
  (->> (registers)
       second
       (sort-by second)
       last
       second))

(defn sol2
  []
  (->> (registers)
       first))
