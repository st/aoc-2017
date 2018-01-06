(ns aoc-2017.day10)

(defn indices
  [coll pos n]
  (take n (->> (range (count coll)) cycle (drop pos))))

(defn mk-re-indexer
  [coll indices]
  (let [mods (zipmap indices (reverse indices))]
    (fn [index]
      (if-let [new-index (mods index)]
        new-index
        index))))

(defn modify
  [coll pos len]
  (let [re-indexer (mk-re-indexer coll (indices coll pos len))]
    (map #(->> % re-indexer (nth coll)) (range (count coll)))))

(defn next-step
  [[coll pos skip] len]
  [(modify coll pos len)
   (mod (+ pos len skip) (count coll))
   (inc skip)])

(defn process-lengthes
  [coll lens]
  (reduce next-step [coll 0 0] lens))

(defn sol
  [coll lens]
  (let [processed (first (process-lengthes coll lens))]
    (* (first processed) (second processed))))

(defn lens-from-bytes
  [s]
  (map int s))

(defn lens-from-bytes-plus
  [s]
  (concat (lens-from-bytes s) [17, 31, 73, 47, 23]))

(defn sparse-hash
  [coll s]
  (let [repeatitions (flatten (repeat 64 (lens-from-bytes-plus s)))]
    (reduce next-step [coll 0 0] repeatitions)))

(defn dense-hash
  [hashes]
  (->> hashes
       (partition 16)
       (map #(apply bit-xor %))))

(defn knot-hash
  [coll s]
  (->> (sparse-hash coll s)
       first
       dense-hash
       (map #(format "%02X" %))
       (apply str)
       (.toLowerCase)))