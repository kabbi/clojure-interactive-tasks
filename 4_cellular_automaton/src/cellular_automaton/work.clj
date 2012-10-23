(ns cellular-automaton.work
  (:use cellular-automaton.core))

;;; Your task is to implement cellular automaton.
;;; The most famous example of cellular automaton is Conway's Game of Life.
;;; Unlike previous tasks now you have to implement visualization and bots. So you need to implement everything :)
;;; I suggest to use quil library for animation (it was used in all previous tasks): https://github.com/quil/quil
;;; But of course you can use whatever you want.
;;; Keep in mind that is should be simple to run your simulator with different automata (Game of Life is only 1 example).


;;; Implement and run Brian's Brain automaton in your simulator: http://en.wikipedia.org/wiki/Brian%27s_Brain


;;; Implement Wireworld automaton: http://en.wikipedia.org/wiki/Wireworld


;;; Add Wireworld implementation to Rosetta Code (it's not present here yet): http://rosettacode.org/wiki/Wireworld


;;; Implement Von Neumann cellular automaton: http://en.wikipedia.org/wiki/Von_Neumann_cellular_automata


;;; Implement Langton's ant: http://en.wikipedia.org/wiki/Langton%27s_ant


;;; Add ability to change cells' states by mouse click, to restart and pause simulation.

(def size-x (quot w @cell-size))
(def size-y (quot h @cell-size))

; Conway's Game of Life (http://en.wikipedia.org/wiki/Conway's_Game_of_Life)

(defn gol-wrap-cells [cells]
	(set (map #(do [(mod (first %) size-x) (mod (second %) size-y)]) cells)))
(defn gol-neighbours [[x y]]
	(gol-wrap-cells
		(for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
			[(+ dx x) (+ dy y)])))

(defn gol-step [cells]
	(set (for [[loc n] (frequencies (mapcat gol-neighbours cells))
			   :when (or (= n 3) (and (= n 2) (cells loc)))]
			loc)))
(defn gol-strip-cell-type [cells]
	(reduce #(if (val %2) (conj % (key %2)) %) #{} cells))
(defn gol-append-cell-type [cells]
	(reduce #(assoc % %2 :on) {} cells))

(defn game-of-life
	([]
		; lets start with a glider!
		(gol-append-cell-type #{[0 2] [1 2] [2 2] [2 1] [1 0]}))
	([prev-state]
		(-> prev-state gol-strip-cell-type gol-step gol-wrap-cells gol-append-cell-type)))
(def gol-cell-colors
	{nil '(0), :on '(255 255 0)})
(defn gol-switch-cell [old-state]
	(if old-state nil :on))

(start-simulation
	game-of-life gol-cell-colors gol-switch-cell)

;;; Brian's Brain (http://en.wikipedia.org/wiki/Brian%27s_Brain)

(defn bb-filter-cells [cells state]
	(reduce #(apply assoc % %2) {} (filter #(= (val %) state) cells)))
(defn bb-filter-dead [cells]
	(reduce #(apply assoc % %2) {} (filter val cells)))

(defn bb-neighbours [[[x y] _]]
	(for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
		[(+ dx x) (+ dy y)]))

(defn bb-substep [cells dead]
	(zipmap (for [[loc n] (frequencies (mapcat bb-neighbours cells))
			   :when (and (= n 2) (not (dead loc)))]
			loc) (repeat :on)))
(defn bb-step [cells]
	(let [dying (bb-filter-cells cells :on)
		  alive (bb-substep dying (bb-filter-cells cells :dying))
		  dying (zipmap (keys dying) (repeat :dying))]
		(merge alive dying)))

(defn brian-brain
	([]
		{})
	([prev-state]
		(-> prev-state bb-filter-dead bb-step)))
(def bb-cell-colors
	{nil '(0), :on '(255 255 255), :dying '(0 0 142)})
(defn bb-switch-cell [old-state]
	(cond
		(= old-state nil) :on
		(= old-state :on) :dying
		(= old-state :dying) nil))

#_(start-simulation
	brian-brain bb-cell-colors bb-switch-cell)

;;; Langton's ant (http://en.wikipedia.org/wiki/Langton%27s_ant)
; NOT YET IMPLEMENTED
#_(
(defn ant-filter-dead [cells]
	(reduce #(apply assoc % %2) {} (filter val cells)))

(defn ant-turn-left []
	)

(defn ant-step [cells]
	(let [dying (ant-filter-cells cells :on)
		  alive (ant-substep dying (ant-filter-cells cells :dying))
		  dying (zipmap (keys dying) (repeat :dying))]
		(merge alive dying)))

(defn langton-ant
	([]
		{})
	([prev-state]
		(-> prev-state ant-filter-dead ant-step)))
(def ant-cell-colors
	{nil '(0), :on '(0 0 0), :ant-on '(255 0 0), :ant-off '(142 0 0)})
(defn ant-switch-cell [old-state]
	(cond
		(= old-state nil) :on
		(= old-state :on) :dying
		(= old-state :dying) nil))

#_(start-simulation
	langton-ant ant-cell-colors ant-switch-cell)
)

;;; Wireworld (http://en.wikipedia.org/wiki/Wireworld)
; NOT YET IMPLEMENTED
#_(
(defn ww-filter-cells [cells state]
	(reduce #(apply assoc % %2) {} (filter #(= (val %) state) cells)))
(defn ww-filter-empty [cells]
	(reduce #(apply assoc % %2) {} (filter val cells)))

(defn ww-neighbours [[[x y] _]]
	(for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
		[(+ dx x) (+ dy y)]))

(defn ww-substep [cells dead]
	(zipmap (for [[loc n] (frequencies (mapcat ww-neighbours cells))
			   :when (and (= n 2) (not (dead loc)))]
			loc) (repeat :on)))
;(defn ww-step [cells]
;	(let [dying (ww-filter-cells cells :on)
;		  alive (ww-substep dying (ww-filter-cells cells :dying))
;		  dying (zipmap (keys dying) (repeat :dying))]
;		(merge alive dying)))
(defn ww-step [cells]
	(zipmap (for [[[loc state] n] (frequencies (mapcat ww-neighbours cells))
			   :when (and (= n 2))]
			loc) (repeat :on)))

(defn wire-world
	([]
		{})
	([prev-state]
		(-> prev-state ww-filter-empty ww-step)))
(def ww-cell-colors
	{nil '(0), :wire '(142 142 0), :head '(0 142 142), :tail '(142 0 0)})
(defn ww-switch-cell [old-state]
	(cond
		(= old-state nil) :wire
		(= old-state :wire) :head
		(= old-state :head) :tail
		(= old-state :tail) nil))

#_(start-simulation
	brian-brain ww-cell-colors ww-switch-cell)
)


