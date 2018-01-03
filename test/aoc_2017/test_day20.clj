(ns aoc-2017.test-day20
  (:require
    [aoc-2017.day20 :refer :all]
    [clojure.test :refer :all]))

(deftest test-parse
  (is (= [3 -4 0] (parse-acc "p=<-4524,4899,1314>, v=<69,-62,-36>, a=<3,-4,0>")))
  (is (= 157 (sol "src/resources/day20.txt"))))
