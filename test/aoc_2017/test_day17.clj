(ns aoc-2017.test-day17
  (:require
    [clojure.test :refer :all]
    [aoc-2017.day17 :refer :all]
    [criterium.core :as c]))

(deftest test-sol
  ;; (c/quick-bench (spin-simple 394 5e7))
  (is (= 10150888 (spin-simple 394 5e7)))
  (is (= 926 (short-circuit 394 2017))))
