(ns k-means.work
  (:use [k-means.core :only [run-empty run-2-circles run-3-circles run-random-circles]]))


;;; Your task is to implement clustering algorithm.
;;; You're a given a set of points on plane. And your goal is to divide them to k clusters.
;;; Implement k-means algorithm to solve this task: http://en.wikipedia.org/wiki/K-means_clustering
;;; Your function must take collection of points. Each point is a vector of x and y.
;;; It must return collection of clusters. Each cluster - collection of points.
;;; E.g. you have 4 points: [0 0] [1 1] [9 9] [10 10] and you need to partition them to 2 clusters.
;;; Input will be [[0 0] [9 9] [1 1] [10 10]] and output should be something like [[[0 0] [1 1]] [[9 9] [10 10]]].
;;; Note that you don't get k - number of clusters. You need to specify it somewhere in function.
;;; To test you solution use following tests:


(defn distance [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (Math/pow (- x1 x2) 2) (Math/pow (- y1 y2) 2))))

(defn closest-center [p c]
    (apply min-key #(distance % p) c))

(defn calculate-new-center [a]
  (map #(/ % (count a)) (reduce #(map + % %2) a)))

(defn k-means [k points]
	(when (<= k (count points))
		(loop [centers (take k (repeatedly #(points (rand-int (count points)))))]			
				(let [clusters (group-by #(closest-center % centers) points)
					  new-centers (map calculate-new-center (vals clusters))]
					  (if (= new-centers centers) ; => converged
					  	(vals (group-by #(closest-center % centers) points))
					  	(recur new-centers))))))

(defn cost-of-clustering [clusters]
	(let [centers (map calculate-new-center clusters)
		  cost (map #(distance % (closest-center % centers)) (partition 2 (flatten clusters)))]
		  (apply + cost)))

(defn pick-best-k-means [k times points]
	(apply min-key cost-of-clustering (take times (repeatedly #(k-means k points)))))

;(run-empty (partial pick-best-k-means 2))

;(run-2-circles (partial pick-best-k-means 2 10))

;(run-3-circles (partial pick-best-k-means 3 10))

;;; Manipulation: mouse click - add new point
;;;               space - reset simulation (remove all points or regenerate them, depenends on test)
;;; Note that may need use different solutions (with k = 2 for run-2-circles and  k = 3 for run-3-circles).



;;; Now try to improve your solution so it can determine k based on given points. So if there are visually 3 clusters it should partition points to 3 clusters, if 4 than to 4 clusters.
;;; Test your solution on this test:

;Epigraf:
;In practice of course, "`optimal quality"' becomes "`whichever clustering algorithm you like to run"', and "`drops dramatically"'
;becomes one of those gigantic hacks that make Principle and Rigor run away crying and hide under their bed.
(defn kinda-very-simplified-elbow-k-means [points]
	(apply min-key #(* (cost-of-clustering %) (count %)) (map #(pick-best-k-means % 10 points) (range 2 8))))

(run-random-circles kinda-very-simplified-elbow-k-means)



;;; Implement some other clustering algorithm.
