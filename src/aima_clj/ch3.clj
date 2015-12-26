(ns aima-clj.ch3)

(defprotocol Problem
  "An abstract formulation of a search problem"
  (initial-state [this] "Initial state in which the agent starts")
  (actions [this state] "Possible actions available to the agent at a state")
  (result [this state action] "The result of taking an action at a state")
  (goal? [this state] "Determines whether a given state is a goal state")
  (step-cost [this state action] "The cost of taking an action in a state"))

(defprotocol Fringe
  "A strategy for inserting and removing nodes from a fringe"
  (insert [this node] "Insert a new node into the fringe")
  (remove-next [this] "Remove the next node from the fringe"))

(defrecord Node [state path cost])

(defn successor
  "Make a successor node from the current node and the next action"
  [problem node action]
  (let [[state path cost] node
        r (result problem state action)
        sc (step-cost problem state action)]
    (Node. r (conj path action) (+ sc cost))))

(defn successors
  "The successor nodes of a given node for a problem"
  [problem node]
  (let [a (actions problem (:state node))]
    (map (partial successor problem node) a)))

(defn tree-search
  "General tree search algorithm"
  [problem fringe]
  (let [start (Node. (initial-state problem) [] 0)]
    (loop [f (insert fringe start)]
      (if-not (empty? f)
        (let [[node f] (remove-next f)]
          (cond (goal? problem (:state node)) (:path node)
                :else (let [s (successors problem node)]
                        (recur (reduce insert f s)))))))))
