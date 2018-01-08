(ns aoc-2017.day14
  (:require
    [clojure.set :as set]
    [aoc-2017.day10 :refer [knot-hash]]))

(defn zero-pad-left
  [n s]
  (let [d (- n (count s))]
    (if (< 0 d)
      (str (apply str (repeat d \0)) s)
      s)))

(defn h->b
  [s]
  (. (BigInteger. s 16)
     (toString 2)))

(defn knot-hash-256
  [s]
  (knot-hash (range 256) s))

(defn row
  [seed n]
  (-> (str seed "-" n)
      knot-hash-256
      h->b))

(defn rows
  [seed]
  (map (partial row seed) (range 128)))

(defn row-usage
  [row]
  (->> row
       (filter #(= \1 %))
       count))

(defn used
  [rows]
  (->> rows
       (map row-usage)
       (reduce +)))

(defn sol
  [seed]
  (used (rows seed)))

(defn ->ij
  [index]
  [(mod index 128) (quot index 128)])

(defn ->index
  [[i j]]
  (+ (* j 128) i))

(defn at
  [coll index]
  (let [[i j] (->ij index)]
    (-> coll (nth j) (nth i))))

(defn make-cell
  [bitrows index]
  [index (at bitrows index) nil])

(defn in?
  [[i j]]
  (and (< -1 i 128) (< -1 j 128)))

(defn present?
  [cell]
  (-> cell second (= \1)))

(defn free?
  [cell]
  (-> cell (nth 2) nil?))


(defn adjacents-indices
  [index]
  (let [[i j] (->ij index)
        up [i (dec j)]
        down [i (inc j)]
        right [(inc i) j]
        left [(dec i) j]]

    (->> [[i j] up down right left]
         (filter in?)
         (map ->index))))

(defn adjacents-cells-present
  [cells index]
  (when (-> cells (nth index) present?)
    (->> (adjacents-indices index)
         (map (partial nth cells))
         (filter present?))))

;; (defn present-adjacents
;;   [cells index avoid level seen]
;;   (println "index" index "level" level)
;;   (if
;;     (or
;;       (seen [index level])
;;       (not (present? (nth cells index))))

;;     #{}
;;     (let [first-level-adjacents (->> (adjacents-indices index avoid)
;;                                      (mapv (partial nth cells))
;;                                      (filter present?)
;;                                      (into #{}))
;;           indices-first-levels (map first first-level-adjacents)
;;           avoid-second (reduce conj avoid indices-first-levels)
;;           second-level (map #(present-adjacents
;;                                cells
;;                                %
;;                                avoid-second
;;                                (inc level)
;;                                (into seen [index level]))
;;                             indices-first-levels)]

;;       (println "count second level" (count second-level))
;;       (->> (reduce #(into %1 %2) first-level-adjacents second-level)
;;            (remove empty?)
;;            (into #{})))))

;; (defn recur-adjacents-indices
;;   [cells index]
;;   (map first (present-adjacents cells index #{} 0 #{})))

(defn make-cells
  [seed]
  (let [bitrows (->> seed
                     rows
                     (map (partial zero-pad-left 128)))]
    (map (partial make-cell bitrows) (range (* 128 128)))))

; (defn change-group
;   [group cells index]
;   (assoc-in (vec cells) [index 2] group))
;
; (defn change-groups
;   [cells indices group]
;   (reduce (fn [cells index] (change-group group cells index)) cells indices))
;
; (defn diffuse
;   [[cells group] index]
;   (println "diffusing " index)
;   (let [cell (nth cells index)]
;     (if (and (present? cell) (free? cell))
;       (let [group-cell (nth cell 2)
;             group-to-use (if group-cell group-cell (inc group))
;             new-group (if group-cell group (inc group))
;             indices-to-touch (map first (present-adjacents cells index #{}))]
;         [(change-groups cells indices-to-touch group-to-use) new-group])
;       [cells group])))

(defn sol2
  [seed]
  (loop [cells (make-cells seed) i 0 nb-group 0 seen #{}]
    (println i)
    (cond
      (= (* 128 128) i) nb-group
      (seen i) (recur cells (inc i) nb-group seen)
      :else
      (let [current-indices (recur-adjacents-indices cells i)]
        (if (empty? current-indices)
          (recur cells (inc i) nb-group seen)
          (recur cells (inc i) (inc nb-group) (into seen current-indices)))))))

;; ljoxqyyw

;; flqrgnkx