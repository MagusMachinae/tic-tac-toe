(ns tic-tac-toe.core)

(def empty-board (->> (repeat :e)
                      (partition 3)
                      (take 3)
                      (mapv vec)))

(def diag-x [[:x :e :e] [:e :x :e] [:e :e :x]])

(get-in diag-x [0 0])

(def left-diag [[0 0]
                [1 1]
                [2 2]])

(def right-diag [[0 2]
                 [1 1]
                 [2 0]])

(some #{(repeat 3 :o)} (mapv (fn [matrix] (mapv #(get-in diag-x %) matrix)) [left-diag right-diag]))
;(mapv (fn [row] (filterv (fn [space] (= player space)) row)) board)

(defn column-vectors
  "transposes board to get column vectors"
  [board]
  (apply mapv vector board))

(defn diag-vectors
  "Returns vectors representing left-diagonal and right-diagonals of board"
  [board]
  (mapv (fn [matrix] (mapv (fn [co-ords] (get-in board co-ords)) matrix))
        [left-diag right-diag]))

(defn winner?
  "Checks all rows, columns, vectors and diagonals in board for player tokens.
  Returns true if a winning triplex is found."
  [board player]
  (if (= (repeat 3 player)
         (some #{(repeat 3 player)} (concat board
                                     (column-vectors board)
                                     (diag-vectors board))))
    true
    false))

(defn draw?
  [board]
  (not (some #{:e} (flatten board))))

(draw? [[:x :o :x]
        [:o :x :e]
        [:x :o :x]])

(defn next-moves
  [board player])

(defn analyse
  [board]
  (cond (winner? board :x) :x
        (winner? board :o) :o
        (draw? board)      :draw
        :else              :ongoing))

(defn game
  [board x y]
  (let [updated-board (update-in board [x y] (fn [_] :o))]
    (if (or (winner? updated-board :o) (draw? updated-board))
      {:board updated-board :state (analyse updated-board)}
      (let [computer-moved (rand-nth (next-moves updated-board :x))]
        {:board computer-moved :state (analyse computer-moved)}))))
