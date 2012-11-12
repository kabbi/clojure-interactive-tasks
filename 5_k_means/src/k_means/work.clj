(ns k-means.work
  (:use [k-means.core :only [run-empty run-2-circles run-3-circles run-random-circles]]))

(def K (atom 3))


;;; Your task is to implement clustering algorithm.
;;; You're a given a set of points on plane. And your goal is to divide them to k clusters.
;;; Implement k-means algorithm to solve this task: http://en.wikipedia.org/wiki/K-means_clustering
;;; Your function must take collection of points. Each point is a vector of x and y.
;;; It must return collection of clusters. Each cluster - collection of points.
;;; E.g. you have 4 points: [0 0] [1 1] [9 9] [10 10] and you need to partition them to 2 clusters.
;;; Input will be [[0 0] [9 9] [1 1] [10 10]] and output should be something like [[[0 0] [1 1]] [[9 9] [10 10]]].
;;; Note that you don't get k - number of clusters. You need to specify it somewhere in function.
;;; To test you solution use following tests:

; (run-empty SOLUTION)

; (run-2-circles SOLUTION)

; (run-3-circles SOLUTION)

;;; Manipulation: mouse click - add new point
;;;               space - reset simulation (remove all points or regenerate them, depenends on test)
;;; Note that may need use different solutions (with k = 2 for run-2-circles and  k = 3 for run-3-circles).

(defn sqr [x]
	(* x x))

(defn dist-sqr [[x1 y1] [x2 y2]]
	(+ (sqr (- x1 x2)) (sqr (- y1 y2))))

(defn choose-starting-clasters [points]
	(take @K (shuffle points)))

(defn which-cluster [clusters point]
	(let [dist-map (zipmap (map #(dist-sqr % point) clusters) clusters)]
		(dist-map (apply min (keys dist-map)))))

(defn average [data]
	(/ (apply + data) (count data)))

(defn map-points-to-clusters [clusters points]
	(reduce #(merge-with conj % {(which-cluster clusters %2) %2})
		(zipmap clusters (repeat []))
		points))

(defn calc-center [points]
	[(average (map first points))
	 (average (map second points))])

(defn move-clusters [clusters points]
	(reduce #(assoc % (calc-center (val %2)) (val %2)) {} (map-points-to-clusters clusters points)))

(defn clusters-stabilized? [old-state new-state]
	(= (set (keys old-state)) (set (keys new-state))))

;;; Currently this is not really finished :)
(defn process-clusters [points]
	(let [clusters (atom (move-clusters (choose-starting-clasters points) points))]
		(while (not (clusters-stabilized? @clusters (move-clusters (keys @clusters) points)))
			(swap! clusters #(move-clusters (keys %) points)))
		@clusters))

(defn k-means [points]
	(vec (vals (process-clusters points))))

;(run-empty k-means)
(run-3-circles k-means)


;;; Now try to improve your solution so it can determine k based on given points. So if there are visually 3 clusters it should partition points to 3 clusters, if 4 than to 4 clusters.
;;; Test your solution on this test:

; (run-random-circles SOLUTION)



;;; Implement some other clustering algorithm.
