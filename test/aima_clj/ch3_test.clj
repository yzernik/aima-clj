(ns aima-clj.ch3-test
  (:require [clojure.test :refer :all]
            [aima-clj.ch3 :refer :all]))

(deftest queens
  (testing "should be correct configuration of queens"
    (is (= [7 3 0 2 5 1 6 4]
           (path (depth-first-tree-search (->NQueensProblem 8)))))))
