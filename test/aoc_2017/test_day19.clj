(ns aoc-2017.test-day19
  (:require
    [aoc-2017.day19 :refer :all]
    [clojure.test :refer :all]))

(deftest test-sols
  (is (= "DTOUFARJQ" (sol1 "src/resources/day19.txt")))
  (is (= 16642 (sol2 "src/resources/day19.txt"))))
