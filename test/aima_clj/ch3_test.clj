(ns aima-clj.ch3-test
  (:require [clojure.test :refer :all]
            [aima-clj.ch3 :refer :all]))

(deftest queens
  (testing "should be correct configuration of queens"
    (is (= [7 3 0 2 5 1 6 4]
           (path (depth-first-tree-search (->NQueensProblem 8)))))))

(deftest npuzzle
  (testing "should find the right solution path for the puzzle"
    (let [p (->NPuzzleProblem [[5 9] [6 nil]])]

      ; bfs
      (is ( = (list [[0 1] [1 0]])
              (path (breadth-first-graph-search p))))

      ; ucs
      (is ( = (list [[0 1] [1 0]])
              (path (uniform-cost-search p)))))))

(def g
  {\a {\b 5 \c 2}
   \b {\e 5}
   \c {\d 2}
   \d {\e 2}})

(deftest graph-problem
  (testing "should find path through the graph"
    (let [p (->GraphProblem g \a \e)]
      (is (= (list \c \d \e)
             (path (depth-first-graph-search p))))
      (is (= (list \b \e)
             (path (breadth-first-graph-search p))))
      (is (= (list \c \d \e)
             (path (uniform-cost-search p)))))))
