(ns aoc-2017.test-day10
  (:require
    [aoc-2017.day10 :refer :all]
    [clojure.test :refer :all]))


(deftest test-indices
  (is (= [3 4 0 1] (indices [0 1 2 3 4] 3 4)))
  (is (= [3 4 0 1] (indices [:a :b :c :d :e] 3 4))))

(deftest test-modify
  (is (= [4 3 2 1 0] (modify [0 1 2 3 4] 3 4))))

(deftest test-process-lengthes
  (is (= [[3 4 2 1 0] 4 4] (process-lengthes [0 1 2 3 4] [3 4 1 5]))))

(deftest test-sol
  (is (= 12 (sol (range 5) [3 4 1 5])))
  (is (= 46600 (sol (range 256) [18,1,0,161,255,137,254,252,14,95,165,33,181,168,2,188]))))

(deftest test-lens-from-bytes
  (is (= [49 44 50 44 51] (lens-from-bytes "1,2,3")))
  (is (= [49 44 50 44 51 17 31 73 47 23] (lens-from-bytes-plus "1,2,3"))))

(deftest test-knot-hash
  (is (= "a2582a3a0e66e6e86e3812dcb672a272" (knot-hash (range 256) "")))
  (is (= "23234babdc6afa036749cfa9b597de1b" (knot-hash (range 256)
                                                       "18,1,0,161,255,137,254,252,14,95,165,33,181,168,2,188"))))
