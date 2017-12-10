(ns aoc-2017.test-day6
  (:require
    [clojure.test :refer :all]
    [aoc-2017.day6 :refer :all]))

(deftest test-sol
  (is (= 5 (gen-size
                [0 2 7 0])))
  (is (= 4 (loop-size
                [0 2 7 0])))
  (is (= 3156 (gen-size
                [2 8 8 5 4 2 3 1 5 5 1 2 15 13 5 14])))
  (is (= 1610 (loop-size
                [2 8 8 5 4 2 3 1 5 5 1 2 15 13 5 14]))))

