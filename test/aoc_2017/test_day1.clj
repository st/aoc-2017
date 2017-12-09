(ns aoc-2017.test-day1
  (:require
    [clojure.test :refer :all]
    [aoc-2017.day1 :refer :all]))

(deftest test-day1
  (is (= 1171 (sol1 input)))
  (is (= 1024 (sol2 input))))

