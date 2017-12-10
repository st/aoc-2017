(ns aoc-2017.day6)

(defn first-max
  [bank]
  (->> bank
       (map-indexed (fn [idx itm] [idx itm]))
       (sort-by (juxt second (comp - first)))
       last))

(defn distribute
  [bank i v]
  (if (zero? v)
    bank
    (let [insertion (mod (inc i) (count bank))]
      (recur (update bank insertion inc)
             insertion
             (dec v)))))

(defn redistribute
  [bank]
  (let [[i v] (first-max bank)]
    (-> bank
        (assoc i 0)
        (distribute i v))))

(defn redistribute-until-seen
  ([exit-fn bank]
   (redistribute-until-seen exit-fn bank {} 1))
  ([exit-fn bank seen gen]
   (let [new-bank (redistribute bank)]
     (if (seen new-bank)
       (exit-fn gen seen new-bank)
       (recur exit-fn new-bank (assoc seen new-bank gen) (inc gen))))))

(def gen-size (partial redistribute-until-seen (fn [gen _ _] gen)))
(def loop-size (partial redistribute-until-seen (fn [gen seen bank] (- gen (seen bank)))))