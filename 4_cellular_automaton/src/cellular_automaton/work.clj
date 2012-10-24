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

#_(start-simulation
	game-of-life gol-cell-colors gol-switch-cell)

;;; Brian's Brain (http://en.wikipedia.org/wiki/Brian%27s_Brain)

(defn bb-filter-cells [cells state]
	(into {} (filter #(= (val %) state) cells)))
(defn bb-filter-dead [cells]
	(into {} (filter val cells)))

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
		; The beautiful pulsar
		{[34 29] :dying, [33 31] :dying, [32 28] :dying, [31 30] :dying,
		 [33 29] :on, [33 30] :on, [32 30] :on, [32 29] :on})
	([prev-state]
		(-> prev-state bb-filter-dead bb-step)))
(def bb-cell-colors
	{nil '(0), :on '(255 255 255), :dying '(0 0 142)})
(defn bb-switch-cell [old-state]
	(cond
		(= old-state nil) :on
		(= old-state :on) :dying
		(= old-state :dying) nil))

(start-simulation
	brian-brain bb-cell-colors bb-switch-cell)

;;; Langton's ant (http://en.wikipedia.org/wiki/Langton%27s_ant)
; NOT YET IMPLEMENTED
#_(
(defn ant-filter-dead [cells]
	(into {} (filter val cells)))

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

(defn ww-filter-cells [cells state]
	(into {} (filter #(= (val %) state) cells)))
(defn ww-filter-empty [cells]
	(into {} (filter val cells)))

(defn ww-neighbours [[x y]]
	(for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
		[(+ dx x) (+ dy y)]))

(defn ww-check-signal [cells cell]
	(let [neighbours (keep cells (ww-neighbours cell))
		  neis-count (count (filter #{:head} neighbours))]
		(if (or (= neis-count 1) (= neis-count 2))
			:head
			:wire)))

(defn ww-step [cells]
	(into {} (for [[loc type] cells]
		(cond
			(= type :head)
				[loc :tail]
			(= type :tail)
				[loc :wire]
			(= type :wire)
				[loc (ww-check-signal cells loc)]))))

(defn wire-world
	([]
		; I've stolen it from Wikipedia
		{[28 27] :wire, [30 29] :wire, [55 23] :wire, [27 27] :head, [29 29] :wire, [54 23] :wire,
		 [26 27] :tail, [28 29] :wire, [53 23] :wire, [55 25] :wire, [27 29] :wire, [52 23] :wire,
		 [54 25] :wire, [25 28] :wire, [26 29] :wire, [53 25] :wire, [55 27] :wire, [51 24] :wire,
		 [52 25] :wire, [54 27] :wire, [53 27] :wire, [50 25] :wire, [51 26] :wire, [52 27] :wire,
		 [49 25] :wire, [48 25] :wire, [47 25] :wire, [46 25] :wire, [45 25] :head, [44 25] :tail,
		 [43 25] :wire, [41 24] :wire, [42 25] :wire, [38 22] :wire, [39 23] :tail, [40 24] :head,
		 [41 25] :wire, [37 22] :wire, [39 24] :head, [41 26] :wire, [36 22] :wire, [38 24] :head,
		 [40 26] :wire, [35 22] :wire, [38 25] :wire, [39 26] :wire, [38 26] :wire, [39 27] :wire,
		 [33 21] :head, [34 22] :wire, [32 21] :tail, [38 28] :wire, [31 21] :wire, [33 23] :wire,
		 [37 28] :wire, [30 21] :wire, [32 23] :wire, [36 28] :wire, [29 21] :wire, [31 23] :wire,
		 [35 28] :wire, [28 21] :wire, [30 23] :wire, [27 21] :wire, [29 23] :tail, [33 27] :head,
		 [34 28] :wire, [26 21] :wire, [28 23] :head, [32 27] :tail, [27 23] :wire, [31 27] :wire,
		 [33 29] :wire, [25 22] :wire, [26 23] :wire, [30 27] :wire, [32 29] :wire, [29 27] :wire,
		 [31 29] :wire})
	([prev-state]
		(-> prev-state ww-filter-empty ww-step)))
(def ww-cell-colors
	{nil '(0), :wire '(246 214 19), :head '(63 128 255), :tail '(223 73 10)})
(defn ww-switch-cell [old-state]
	(cond
		(= old-state nil) :wire
		(= old-state :wire) :head
		(= old-state :head) :tail
		(= old-state :tail) nil))

#_(start-simulation
	wire-world ww-cell-colors ww-switch-cell)


