(ns aoc-2017.testday15
  (:require
    [aoc-2017.day15 :refer :all]
    [clojure.test :refer :all]))

(deftest test-matches?
  (is (matches? [245556042  1431495498]))
  (is (not (matches? [245556042  1431495491]))))

(deftest test-nb-matches
  (is (= 588 (sol1 65 16807 8921 48271 4e7)))
  (is (= 650 (sol1 783 16807 325 48271 4e7)))

  (is (= 309 (sol2 65 16807 8921 48271 5e6)))
  (is (= 336 (sol2 783 16807 325 48271 5e6))))