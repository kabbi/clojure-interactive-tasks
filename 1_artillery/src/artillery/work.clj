(ns artillery.work
  (:use [artillery.core :only (plane-static plane-dynamic ufo-static ufo-dynamic)]))


;;; You goal is to hit plane my missile.
;;; Plane always starts at position x = 0, y = 500.
;;; Plane's speed equal to 5.
;;; Plane flies from the left to the right. So it's positions will be (0, 500), (5, 500), (10, 500), etc...
;;; You position is x = 400, y = 0.
;;; Missile speed is 10.
;;; You goal is to calculate what angle you need to launch missile at in order to hit the plane.
;;; You solution is a function that takes no paremeters (constant function) and returns this angle.

;;; Here is an example of such function.
;;; It always returns PI / 2 (missile is launched straight up).
;;; You can either calculate answer or find it by trying and adjusting different angles.
(defn plane-static-solution []
  (let [angleSin (/ 500 (* 2 259))
        angle (- Math/PI (Math/asin angleSin))]
  angle))

;;; Here's a function that will show you animation with plane you launching missiles.
;;; You need to pass your solution (function name) to this function and run this file.
(plane-static plane-static-solution)



;;; Your goal is the same but now plane start at random position.
;;; And your position also changes every second.
;;; So only plane's speed and missiles' speed are known for sure.
;;; You need to write a function that takes 4 numbers - your coordinates (player) and plane's coordinates (target).
;;; Function should calculate angle to launch missile at.

;;; Example
;;; pl-x, pl-y - player's (your) coordinates.
;;; trg-x trg-y - target's coordinates.
;;; Run and see how it launches missile now and then fix it to hit the plane.

; algorithm was calculated using some magic mathematical structures:
; a pen and a copybook (and some spare time on mathematical analysis lecture)
(defn solve-equation
  [a b c]
  (let [d (- (* b b) (* 4 a c))]
    (/ (- (Math/sqrt d) b) (* 2 a))))
(defn calc-plane-x
  [plane-y player-x]
  (let [b (* 2 player-x)
        c (+ (* player-x player-x) (* plane-y plane-y))]
  (solve-equation 3 b (- c))))
(defn calc-angle
  [x y]
  (Math/asin (/ y 2 x)))
(defn plane-dynamic-solution [pl-x pl-y trg-x trg-y]
  (let [plane-x (calc-plane-x trg-y pl-x)
        angle (calc-angle plane-x trg-y)
        angle (if (> plane-x pl-x) angle (- Math/PI angle))]
  angle))

;;; To run program uncomment - remove ';' symbol before '(plane-dynamic ...)'
;;; And also comment previous task - add ';' symbol before '(plane-static ...)'
;(plane-dynamic plane-dynamic-solution)



;;; Now you need to hit UFO.
;;; You're lucky it's not changing, just hanging in the air.
;;; But now gravity force is enabled so your missile won't fly in a straight but rather in a curve. Remember Worms? :)
;;; Gravity force is that missile's y speed will decrease by 0.1 every moment.
;;; UFO position x = 500, y = 300.
;;; UFO speed is equal to 0 (it's not moving).
;;; Your position x = 0, y = 0.
;;; Missile speed stays the same as before.
;;; You need to write function that takes no arguments and returns angle to launch missile at.

;;; Now you don't have template function, so write one yourself.
;;; Hint: try to pass random angle at first e.g. 0.5 and see how it works.
;;; To run program uncomment it (and comment others) and pass your function to it.
(defn ufo-solution []
  (* (rand) Math/PI))
;(ufo-static ufo-solution)



;;; Same UFO, but now it appears at random position (same as plane-dynamic).
;;; Your position is also changing.
;;; You need to write function that takes 4 arguments: your position (x, y)  and UFO's position (x, y).
(defn ufo-dynamic-solution [pl-x pl-y trg-x trg-y]
  (* (rand) Math/PI))
;(ufo-dynamic ufo-dynamic-solution)



;;; If you're still full of energy I propose you to add wind to simulation.
;;; Open core.clj file and try to figure out (it's not very easy) where missile speed is changed and try to add wind.
;;; And solve tasks with new obstacles.
