(ns aoc-2017.test-day8
  (:require
    [clojure.test :refer :all]
    [aoc-2017.day8 :refer :all]))

(deftest test-sol
  (is (= 4448 (sol1)))
  (is (= 6582 (sol2))))

