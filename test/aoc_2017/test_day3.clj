(ns aoc-2017.test-day3
  (:require
    [clojure.test :refer :all]
    [aoc-2017.day3 :refer :all]))

(deftest test-sol
  (is (= 480 (sol 347991))))

(deftest test-sol2
  (is (= 349975 (sol2 347991))))