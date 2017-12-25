(ns aoc-2017.test-day16
  (:require
    [aoc-2017.day16 :refer :all]
    [clojure.test :refer :all]))

;; s1, a spin of size 1: eabcd.
;; x3/4, swapping the last two programs: eabdc.
;; pe/b, swapping programs e and b: baedc.

(deftest test-shift
  (is (= "eabcd" (dance "abcde" "s1" )))
  (is (= "eabdc" (dance "eabcd" "x3/4" )))
  (is (= "baedc" (dance "eabdc" "pe/b" )))

  (is (= "baedc" (all-dances "abcde" ["s1"
                                      "x3/4"
                                      "pe/b"]))))

(deftest test-sol
  (is (= "dcmlhejnifpokgba" (sol))))