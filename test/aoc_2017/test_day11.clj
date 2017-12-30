(ns aoc-2017.test-day11
  (:require
    [aoc-2017.day11 :refer :all]
    [clojure.test :refer :all]))

(deftest test-sol
  (is (= 812 (sol1)))
  (is (= 1603 (sol2)))
  (is (= 1603 (sol3))))

