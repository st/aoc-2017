(ns aoc-2017.test-day7
  (:require
    [clojure.test :refer :all]
    [aoc-2017.day7 :refer :all]))

(deftest test-path
  (is (nil? (path [:a] [:zzz])))
  (is (= [0] (path [:a] [:a])))
  (is (= [0] (path [:a
                    [[:b]]] [:a])))
  (is (= [1 0] (path [:a
                      [[:b]]] [:b])))
  (is (= [1 1] (path [:a [[:z]
                          [:b]]] [:b])))
  (is (= [1 1 0] (path [:a [[:z]
                            [:f
                             [[:b]]]]] [:b]))))

