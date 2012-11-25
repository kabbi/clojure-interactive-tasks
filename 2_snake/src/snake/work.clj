(ns snake.work
  (:use [snake.core :only (run-not-grow run-grow run-many-apples run-with-walls)]))

;;; You're writing a bot for playing snake.
;;; So, you are a snake and your goal is to collect apples.
;;; Field sizes: 40 x 30
;;; Every turn you move to one of the neighbours cell.
;;; Your function must take 2 arguments: snake's position and apple's position and decide which direction to move.
;;; Directions are: :up, :down, :left, :right (they are keywords). Your function must return one of these directions.
;;; Position (snake's or apple's) is a vector of 2 elements: x and y.
;;; In this task snake is not growing after it ate an apple so there is no danger of snake hitting itself.
;;; Note: upper left corner cell is (0, 0).

;;; Uncomment and substitute your solution
(defn snake-not-grow-solution [snakePos applePos]
	(let [[sX sY] (first snakePos)
		  [aX aY] applePos]
		(cond 
			(< sX aX) :right
			(> sX aX) :left
			(< sY aY) :down
			(> sY aY) :up)))
;(run-not-grow snake-not-grow-solution)



;;; Snake grows now (each time snake eats an apple, it's length increases).
;;; You need to write similar function as in previous task.
;;; It takes 2 arguments.
;;; First argument is snake's body - collection of cells, each cell is a vector of x and y. First cell is snake's head.
;;; Second argument is apple's position - vector of x and y.
;;; It should return direction: :up, :down, :left or :right.
;;; Note that you cannot change direction to the opposite in 1 move: snake will hit it's tail if length is 2 or more.
;;; Wait, you can change direction but snake will die :\

;;; Uncomment and substitute your solution

(defn neighbours
	"Returns the list of neighbours"
	[[x y] v]
	(if (= v -1)
		'() ; no wall neighbours
		(for [dx [-1 0 1] dy [-1 0 1] :when (or (zero? dx) (zero? dy))]
			[(+ dx x) (+ dy y)])))
(defn new-neighbours
	"Returns the list of neighbours with an appropriate wave idx"
	[cell wave]
	(zipmap (apply neighbours cell) (repeat wave)))
(defn wrap
	"Wraps the coords around the board position
	Constants are taken from core.clj"
	[[x y]]
	[(mod x 40) (mod y 30)])
(defn next-wave
	"Gets the next state of the world after the wave iteration
	cells - the world, new-cells - current cells with wave front"
	[cells wave]
	(merge-with min
		cells
		(into {} (for [cell cells] (new-neighbours cell (inc wave))))))
(defn select-next-cell
	"Returns the neighbour cell with minimal wave, excluding walls"
	[cells current]
	(let [neis (new-neighbours [current 0] 0)
		  cells (filter #(neis (first %)) cells)
		  cells (filter #(not= (second %) -1) cells)]
		(first (apply min-key second cells))))
(defn get-path
	"Returns the the next path node from the complete map"
	[cells target]
	;(println "Started getting path")
	;(println cells)
	(loop [cur-cell target prev-cell target]
		(if (zero? (cells cur-cell))
			prev-cell
			(recur (select-next-cell cells cur-cell) cur-cell))))
(defn dfs
	"Returns the next path point
	Accepts the map of cells (-1 - wall, 0 - start, others - air)"
	[cells target]
	(loop [cells cells wave 0]
		(let [wrapped-cells (map wrap (keys cells))
			  target-map (zipmap wrapped-cells (keys cells))]
			(cond
				(> wave 1200)
					target
				;(target-map target)
				;	(get-path cells (target-map target))
				(cells target)
					(get-path cells target)
				:else
					(recur (next-wave cells wave) (inc wave))))))
(defn snake-grow-solution [body applePos]
	(let [head (first body)
		  cells (zipmap body (repeat -1))
		  cells (assoc cells head 0)
		  [sX sY] head
		  [aX aY] (dfs cells applePos)]
		(cond 
			(< sX aX) :right
			(> sX aX) :left
			(< sY aY) :down
			(> sY aY) :up)))
;(run-grow snake-grow-solution)



;;; Now you have many apples (5) instead of one.
;;; Function the same as previous but it takes set of apples instead of the single apple.
;;; Each apple in the set is a vector of x and y.
;;; E.g. you can try to reach nearest apple to the snake.

;;; Uncomment and substitute your solution
(defn dist-sq [p1 p2]
	(->> (map - p1 p2) (map #(* % %)) (reduce +)))
(defn snake-grow-many-apples-solution [body apples]
	(let [head (first body)
		  cells (zipmap body (repeat -1))
		  cells (assoc cells head 0)
		  nearest-apple (apply min-key (partial dist-sq head) apples)
		  [sX sY] head
		  [aX aY] (dfs cells nearest-apple)]
		(cond 
			(< sX aX) :right
			(> sX aX) :left
			(< sY aY) :down
			(> sY aY) :up)))
; (run-many-apples YOUR_SOLUTION_HERE)



;;; Walls are added. So snake can hit wall and die.
;;; Your function now takes third argument - set of walls.
;;; Each wall is a cell that snake is not allowed to  move to.
;;; Wall is a vector of x and y.

;;; Uncomment and substitute your solution
(defn snake-grow-many-apples-walls-solution [body apples walls]
	(let [head (first body)
		  cells (zipmap body (repeat -1))
		  cells (merge cells (zipmap walls (repeat -1)))
		  cells (assoc cells head 0)
		  nearest-apple (apply min-key (partial dist-sq head) apples)
		  [sX sY] head
		  [aX aY] (dfs cells nearest-apple)]
		(cond 
			(< sX aX) :right
			(> sX aX) :left
			(< sY aY) :down
			(> sY aY) :up)))
(run-with-walls snake-grow-many-apples-walls-solution)