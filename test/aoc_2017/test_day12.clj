(ns aoc-2017.test-day12
  (:require
    [aoc-2017.day12 :refer :all]
    [clojure.test :refer :all]))

(deftest test-sol
  (is (= 115 (sol "src/resources/day12.txt")))
  (is (= 221 (sol2 "src/resources/day12.txt"))))
