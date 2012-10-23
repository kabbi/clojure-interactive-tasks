(ns cellular-automaton.core
  (:use quil.core))

(def w 800)
(def h 600)
(def cell-size (atom 10))
(defn board-width []
  (/ w @cell-size))
(defn board-height []
  (/ h @cell-size))


(defn setup [state-fn]
  (frame-rate 24)
  (smooth)
  (background 0)
  (fill 0)
  (stroke 0)
  (stroke-weight 1)
  (text-mode :screen)
  (text-align :center)
  (set-state! :running? (atom nil)
              :draw-grid? (atom true)
              :current-state (atom (state-fn))))

(defn to-real-coords [cell]
  (map #(* @cell-size %) cell))
(defn to-board-coords [cell]
  (map #(int (/ % @cell-size)) cell))

(defn draw-cell [cell-state cell-colors cell]
  (let [[real-x real-y] (to-real-coords cell)]
    (no-stroke)
    (apply fill (cell-colors cell-state))
    (rect real-x real-y @cell-size @cell-size)))

(defn draw [state-fn cell-colors]
  (let [running?        (state :running?)
        draw-grid?      (state :draw-grid?)
        current-state   (state :current-state)]
    (background 0)
    (doseq [[cell state] @current-state]
      (draw-cell state cell-colors cell))
    (stroke 100 100)
    (stroke-weight 1)
    (when @draw-grid?
      (doseq [x (range 0 w @cell-size)]
        (line x 0 x h))
      (doseq [y (range 0 h @cell-size)]
        (line 0 y w y)))
    (if @running?
      (swap! current-state state-fn)
      (text "Paused" (/ w 2) (/ h 2)))))

(defn key-handler [state-fn]
  (let [running?      (state :running?)
        draw-grid?    (state :draw-grid?)
        current-state (state :current-state)
        pressed-key   (raw-key)]
    (cond
      (= pressed-key \space)
        (swap! running? not)
      (= pressed-key \g)
        (swap! draw-grid? not)
      (= pressed-key \n)
        (swap! current-state state-fn)
      (= pressed-key \r)
        (reset! current-state (state-fn))
      (= pressed-key \c)
        (reset! current-state {})
      (= pressed-key \p)
        (println @current-state))))
(defn mouse-handler [switch-cell-fn]
  (let [current-state   (state :current-state)
        mouse-coord     [(mouse-x) (mouse-y)]
        cell-coord      (to-board-coords mouse-coord)
        current-value   (@current-state cell-coord)
        new-value       (switch-cell-fn current-value)
        pressed-button  (mouse-button)]
    (cond
      (= pressed-button :left)
        (swap! current-state assoc cell-coord new-value))
      #_(= pressed-button :right)
        #_(swap! current-state assoc cell-coord nil)))

(defn simulate [state-fn cell-colors switch-cell-fn]
  (sketch 
    :title "Cellular Automata"
    :setup #(setup state-fn)
    :draw #(draw state-fn cell-colors)
    :key-pressed #(key-handler state-fn)
    :mouse-clicked #(mouse-handler switch-cell-fn)
    :size [w h]))

(def start-simulation simulate)
