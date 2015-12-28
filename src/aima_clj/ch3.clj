(ns aima-clj.ch3
  (:use clojure.data.priority-map))

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

(defn- successor
  "Make a successor node from the current node and the next action"
  [problem node action]
  (let [{state :state path :path cost :cost} node
        r (result problem state action)
        sc (step-cost problem state action)]
    (->Node r (conj path action) (+ sc cost))))

(defn- successors
  "The successor nodes of a given node for a problem"
  [problem node]
  (let [a (actions problem (:state node))]
    (map (partial successor problem node) a)))

(defn tree-search
  "General tree search algorithm"
  [problem fringe]
  (let [start (->Node (initial-state problem) [] 0)]
    (loop [f (insert fringe start)]
      (if-not (empty? f)
        (let [[node f] (remove-next f)
              {state :state path :path} node]
          (cond (goal? problem state) path
                :else (let [s (successors problem node)]
                        (recur (reduce insert f s)))))))))

(defn graph-search
  "General graph search algorithm"
  [problem fringe]
  (let [start (->Node (initial-state problem) [] 0)]
    (loop [f (insert fringe start)
           c #{}]
      (if-not (empty? f)
        (let [[node f] (remove-next f)
              {state :state path :path} node]
          (cond (goal? problem state) path
                (c state) (recur f c)
                :else (let [s (successors problem node)]
                        (recur (reduce insert f s) (conj c state)))))))))

(defn path-states
  "Show the intermediate states along the solution path"
  [problem path]
  (reductions #(result problem %1 %2) (initial-state problem) path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(extend-type clojure.lang.IPersistentList
  Fringe
  (insert [this node] (conj this node))
  (remove-next [this] [(first this) (rest this)]))

(defn depth-first-tree-search
  [problem]
  (tree-search problem ()))

(defn depth-first-graph-search
  [problem]
  (graph-search problem ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(extend-type clojure.lang.PersistentQueue
  Fringe
  (insert [this node] (conj this node))
  (remove-next [this] [(peek this) (pop this)]))

(defn breadth-first-tree-search
  [problem]
  (tree-search problem clojure.lang.PersistentQueue/EMPTY))

(defn breadth-first-graph-search
  [problem]
  (graph-search problem clojure.lang.PersistentQueue/EMPTY))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(extend-type clojure.data.priority_map.PersistentPriorityMap
  Fringe
  (insert [this node] (conj this [node (:cost node)]))
  (remove-next [this] [(first (peek this)) (pop this)]))

(defn uniform-cost-graph-search
  [problem]
  (graph-search problem (priority-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- conflict?
  "Are the two points conflicting"
  [pos1 pos2]
  (let [[row1 col1] pos1
        [row2 col2] pos2]
    (or (= row1 row2)
        (= col1 col2)
        (= (- row1 col1) (- row2 col2))
        (= (+ row1 col1) (+ row2 col2)))))

(defn- valid-column?
  "Is the column location of the new queen valid"
  [state col]
  (let [positions (map-indexed vector state)
        new-pos [(count state) col]]
    (not (some (partial conflict? new-pos) positions))))

(defrecord NQueensProblem [n]
  Problem
  (initial-state [this] [])
  (actions [this state] (filter (partial valid-column? state) (range n)))
  (result [this state action] (conj state action))
  (goal? [this state] (= n (count state)))
  (step-cost [this state action] 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def deltas [[1 0] [0 1] [-1 0] [0 -1]])

(defn- pieces
  [board]
  (for [i (range (count board))
        j (range (count (board i)))
        :when (get-in board [i j])]
    [i j]))

(defn- add-delta
  [position delta]
  (map + position delta))

(defn- on-board?
  [board position]
  (not= :off-board
      (get-in board position :off-board)))

(defn- empty-position?
  [board position]
  (not (get-in board position)))

(defn- valid-move?
  [board position delta]
  (let [new-pos (add-delta position delta)]
    (and (on-board? board new-pos)
         (empty-position? board new-pos))))

(defn- moves
  [board position]
  (->> deltas
       (filter #(valid-move? board position %))
       (map #(vec [position %]))))

(defn- all-moves
  [board]
  (for [position (pieces board)
        move (moves board position)
        :when move]
    move))

(defn- swap-positions
  [board pos1 pos2]
  (let [v1 (get-in board pos1)
        v2 (get-in board pos2)]
    (-> board
        (assoc-in pos2 v1)
        (assoc-in pos1 v2))))

(defn- move-piece
  [board position delta]
  (let [other-pos (add-delta position delta)]
    (swap-positions board position other-pos)))

(defn- solved?
  [board]
  (apply < (remove nil? (flatten board))))

(defrecord NPuzzleProblem [board]
  Problem
  (initial-state [this] board)
  (actions [this state] (all-moves state))
  (result [this state action] (move-piece state (action 0) (action 1)))
  (goal? [this state] (solved? state))
  (step-cost [this state action] 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord GraphProblem [graph initial goal]
  Problem
  (initial-state [this] initial)
  (actions [this state] (keys (graph state)))
  (result [this state action] action)
  (goal? [this state] (= goal state))
  (step-cost [this state action] ((graph state) action)))

(def romania
  {\A {\Z 75 \S 140 \T 118}
   \B {\U 85 \P 101 \G 90 \F 211}
   \C {\D 120 \R 146 \P 138}
   \D {\M 75}
   \E {\H 86}
   \F {\S 99}
   \H {\U 98}
   \I {\V 92 \N 87}
   \L {\T 111 \M 70}
   \O {\Z 71 \S 151}
   \P {\R 97}
   \R {\S 80}
   \U {\V 142}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
