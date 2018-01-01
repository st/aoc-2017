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

(defn move-cpt-twice
  [m op a b]
  (if (:waiting m)
    m
    (move-cpt m op a b)))

(defn execute-line
  [memory line]
  (let [[cmd a b] (s/split line #" ")]
    (-> memory
        (command cmd a b)
        (move-cpt cmd a b))))

(defn finished?
  [m]
  (< 0 (get m :rcv 0)))

(defn execute-lines
  ([lines]
   (execute-lines lines {:cpt 0}))
  ([lines memory]
   (let [line (nth lines (:cpt memory))
         new-m (execute-line memory line)]
     (if (finished? new-m)
       new-m
       (recur lines new-m)))))

(defmulti command-twice (fn [_ op _ _] op))

(defmethod command-twice "snd"
  [m _ _ _]
  m)

(defmethod command-twice "rcv"
  [m _ _ _]
  m)

(defmethod command-twice :default
  [m op a b]
  (if (:waiting m)
    m
    (command m op a b)))

(defn send
  [m other-m cmd a]
  (if (= "snd" cmd)
    [(update m :nb-send inc)
     (-> other-m
         (update :received #(conj % (register-value m a)))
         (dissoc :waiting))]
    [m other-m]))

(defn receive
  [m cmd a]
  (if (= "rcv" cmd)
    (if-let [first-received (-> m :received first)]
      (-> m
          (assoc a first-received)
          (update :received #(-> % rest vec)))
      (assoc m :waiting true))
    m))

(defn execute-line-twice
  [m0 m1 line0 line1]
  (let [[cmd0 a0 b0] (s/split line0 #" ")
        [cmd1 a1 b1] (s/split line1 #" ")

        [m0 m1] (send m0 m1 cmd0 a0)
        [m1 m0] (send m1 m0 cmd1 a1)

        m0 (receive m0 cmd0 a0)
        m1 (receive m1 cmd1 a1)

        [m0 m1] [(command-twice m0 cmd0 a0 b0) (command-twice m1 cmd1 a1 b1)]]
    [(move-cpt-twice m0 cmd0 a0 b0) (move-cpt-twice m1 cmd1 a1 b1)]))

(defn execute-lines-twice
  ([lines]
   (execute-lines-twice lines {:cpt 0 "p" 0 :received [] :nb-send 0} {:cpt 0 "p" 1 :received [] :nb-send 0}))
  ([lines m0 m1]
   (let [line0 (nth lines (:cpt m0))
         line1 (nth lines (:cpt m1))
         [new-m0 new-m1] (execute-line-twice m0 m1 line0 line1)]
     (if (every? :waiting [new-m0 new-m1])
       [new-m0 new-m1]
       (recur lines new-m0 new-m1)))))

(defn sol
  []
  (-> "src/resources/day18.txt"
      slurp
      (s/split #"\n")
      execute-lines
      :rcv))

(defn sol2
  []
  (-> "src/resources/day18.txt"
      slurp
      (s/split #"\n")
      execute-lines-twice
      second
      :nb-send))