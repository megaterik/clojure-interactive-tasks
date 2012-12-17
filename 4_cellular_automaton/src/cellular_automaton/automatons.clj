(ns cellular-automaton.automatons 
	(:use [cellular-automaton.work :only (run draw-cell draw-ant)]
	  quil.core))


;;;Game of Life
(defn conway-draw-fn [[cell type] cell-size center-shift]
  (condp = type
  	:alive (fill 0 255 0)
  	:dead (fill 0 0 0))
  (draw-cell rect cell cell-size center-shift))

(defn conway-mouse-fn [type]
	(condp = type
		nil :alive
		:alive :dead
		:dead :alive))

(defn conway-process-fn [self neightbors]
	;(println "neightbors " neightbors)
	(let [alive-neightbors (count (filter #( = :alive (second %)) neightbors))]
		(if (or (and (= 2 alive-neightbors) (= self :alive)) (= 3 alive-neightbors)) :alive :dead)))


;;; Brian's Brain automaton http://en.wikipedia.org/wiki/Brian%27s_Brain
(defn brian-draw-fn [[cell type] cell-size center-shift]
	(condp = type
		:dying (fill 0 0 255)
		:alive (fill 255 255 255)
		:dead (fill 0 0 0))
	(draw-cell rect cell cell-size center-shift))

(defn brian-mouse-fn [type]
	(condp = type
		nil :alive
		:dead :alive
		:alive :dying
		:dying :dead))

(defn brian-process-fn [self neightbors]
	(condp = self
		:alive :dying
		:dying :dead
		:dead (if (= 2 (count (filter #(= :alive (second %)) neightbors))) :alive :dead)))

;;;Wireworld automaton: http://en.wikipedia.org/wiki/Wireworld
(defn wireworld-draw-fn [[cell type] cell-size center-shift]
	(condp = type
		:dead (fill 0 0 0)
		:conductor (fill 255 255 0)
		:head (fill 0 0 255)
		:tail (fill 255 0 0))
	(draw-cell rect cell cell-size center-shift))

(defn wireworld-mouse-fn [type]
	(condp = type
		nil :conductor
		:dead :conductor
		:conductor :head
		:head :tail
		:tail :dead))

(defn wireworld-process-fn[self neightbors]
	(condp = self
		:dead :dead
		:head :tail
		:tail :conductor
		:conductor (let [heads (count (filter #(= :head (second %)) neightbors))]
			(if (or (= 1 heads) (= 2 heads)) :head :conductor))))


;;; Langton's ant: http://en.wikipedia.org/wiki/Langton%27s_ant
(defn ant-draw-fn [[cell type] cell-size center-shift]\
	(cond
		(or (= type :dead) (and (vector? type) (= (second type) :black))) (fill 0 0 0)
		(or (= type :white) (and (vector? type) (= (second type) :white))) (fill 255 255 255))
	(draw-cell rect cell cell-size center-shift)
	(when (vector? type) (draw-ant (first type) cell cell-size center-shift))) ;because every ce;; except ant represented by a keyword 

(defn ant-mouse-fn [type]
	(condp = type
		nil [:up :black :ant]
		:dead [:up :black :ant]
		[:up :black :ant] [:right :black :ant]
		[:right :black :ant] [:down :black :ant] 
		[:down :black :ant] [:left :black :ant]
		[:left :black :ant] [:up :white :ant]
		[:up :white :ant] [:right :white :ant]
		[:right :white :ant] [:down :white :ant]
		[:down :white :ant] [:left :white :ant]
		[:left :white :ant] :white
		:white :dead))

(defn ant-process-fn [self neightbors]
	;(println "process-fn for " self " with " neightbors)
	(let [neightbors (into {} (map (partial apply array-map) neightbors))
		  not-nil-self (if (= :white self) :white :black)]
		(if (or (= self :white) (= self :dead))
			(cond
				(= (neightbors [1.0 0.0 ]) [:right :black :ant]) [:down not-nil-self :ant]
				(= (neightbors [-1.0 0.0]) [:left :black :ant])  [:up not-nil-self :ant]
				(= (neightbors [0.0 1.0 ]) [:down :black :ant])  [:left not-nil-self :ant]
				(= (neightbors [0.0 -1.0]) [:up :black :ant])	 [:right not-nil-self :ant]
				(= (neightbors [1.0 0.0 ]) [:left :white :ant])  [:down not-nil-self :ant]
				(= (neightbors [-1.0 0.0]) [:right :white :ant]) [:up not-nil-self :ant]
				(= (neightbors [0.0 1.0 ]) [:up :white :ant])    [:left not-nil-self :ant]
				(= (neightbors [0.0 -1.0]) [:down :white :ant])  [:right not-nil-self :ant]
				:else self) ;stay black or white
			(let [[direction color _] self]
				(if (= color :white) :dead :white)))))


; R for random initial configuration
; Left Mouse Click to change state
; Q-E to decrease/increase scale
; Space to stop/start
#_(run {:draw-fn wireworld-draw-fn :process-fn wireworld-process-fn :mouse-fn wireworld-mouse-fn
	:initial-configurations [
	{'(-2.0 0.0) :conductor, '(2.0 -2.0) :conductor, '(0.0 -2.0) :conductor, '(-1.0 -1.0) :conductor, '(-2.0 -4.0) :conductor, '(1.0 -2.0) :conductor, '(-5.0 -5.0) :conductor, '(-12.0 -6.0) :conductor, '(-3.0 -3.0) :conductor, '(-16.0 1.0) :head, '(-8.0 1.0) :conductor, '(-16.0 2.0) :dead, '(-4.0 1.0) :conductor, '(8.0 -2.0) :conductor, '(4.0 -2.0) :conductor, '(0.0 -1.0) :conductor, '(-2.0 -1.0) :conductor, '(-11.0 -6.0) :conductor, '(-15.0 0.0) :conductor, '(-15.0 2.0) :conductor, '(-15.0 -4.0) :conductor, '(-4.0 -5.0) :conductor, '(-10.0 -6.0) :conductor, '(-7.0 1.0) :conductor, '(-14.0 0.0) :conductor, '(-14.0 2.0) :conductor, '(7.0 -2.0) :conductor, '(-14.0 -4.0) :conductor, '(-16.0 -5.0) :head, '(-8.0 -5.0) :conductor, '(-9.0 -6.0) :conductor, '(-13.0 0.0) :conductor, '(-13.0 2.0) :conductor, '(-13.0 -4.0) :conductor, '(-2.0 -3.0) :conductor, '(0.0 -3.0) :conductor, '(-1.0 -3.0) :conductor, '(-6.0 1.0) :conductor, '(-12.0 0.0) :conductor, '(-12.0 2.0) :conductor, '(-3.0 -1.0) :conductor, '(-3.0 1.0) :conductor, '(6.0 -2.0) :conductor, '(-12.0 -4.0) :conductor, '(-3.0 -2.0) :conductor, '(3.0 -2.0) :conductor, '(-7.0 -5.0) :conductor, '(-11.0 0.0) :conductor, '(-11.0 2.0) :conductor, '(-11.0 -4.0) :conductor, '(-15.0 -6.0) :conductor, '(-5.0 1.0) :conductor, '(-10.0 0.0) :conductor, '(-10.0 2.0) :conductor, '(5.0 -2.0) :conductor, '(-10.0 -4.0) :conductor, '(-6.0 -5.0) :conductor, '(-3.0 -5.0) :conductor, '(-14.0 -6.0) :conductor, '(-9.0 0.0) :conductor, '(-9.0 2.0) :conductor, '(-9.0 -4.0) :conductor, '(-13.0 -6.0) :conductor}
	]})

(run {:draw-fn conway-draw-fn :process-fn conway-process-fn :mouse-fn conway-mouse-fn
	:initial-configurations [
	{'(-4.0 4.0) :alive, '(-3.0 3.0) :alive, '(-4.0 2.0) :alive, '(-3.0 4.0) :alive, '(-5.0 1.0) :dead, '(-5.0 4.0) :alive}
	{'(-4.0 -8.0) :dead, '(-2.0 -8.0) :alive, '(0.0 -8.0) :dead, '(1.0 -8.0) :dead, '(-1.0 -8.0) :alive, '(8.0 1.0) :alive, '(8.0 -1.0) :alive, '(8.0 0.0) :alive, '(-2.0 10.0) :alive, '(-1.0 10.0) :alive, '(-3.0 -8.0) :alive, '(-10.0 -1.0) :alive, '(-10.0 1.0) :alive, '(-10.0 -2.0) :dead, '(-10.0 0.0) :alive, '(-5.0 -8.0) :dead, '(-3.0 10.0) :alive}
	{'(-2.0 2.0) :alive, '(-1.0 2.0) :alive, '(-4.0 -1.0) :dead, '(-4.0 1.0) :alive, '(-4.0 0.0) :alive, '(-4.0 2.0) :alive, '(0.0 1.0) :alive, '(0.0 -1.0) :alive, '(-3.0 -1.0) :alive, '(-3.0 2.0) :alive, '(-5.0 1.0) :dead, '(-5.0 0.0) :dead, '(-5.0 2.0) :dead}
	{'(-4.0 -4.0) :alive, '(4.0 -4.0) :alive, '(2.0 -4.0) :alive, '(4.0 -8.0) :alive, '(2.0 -8.0) :alive, '(-20.0 -5.0) :alive, '(-10.0 -5.0) :alive, '(12.0 -6.0) :dead, '(6.0 -6.0) :dead, '(14.0 -7.0) :alive, '(-7.0 -7.0) :alive, '(-8.0 -1.0) :alive, '(-16.0 -4.0) :dead, '(-19.0 -5.0) :alive, '(4.0 -9.0) :alive, '(-18.0 -5.0) :dead, '(13.0 -7.0) :dead, '(-15.0 -4.0) :dead, '(-4.0 -5.0) :alive, '(0.0 -5.0) :alive, '(1.0 -5.0) :alive, '(-10.0 -3.0) :alive, '(10.0 -6.0) :dead, '(-5.0 -6.0) :alive, '(-7.0 -1.0) :alive, '(-16.0 -5.0) :dead, '(-9.0 -6.0) :alive, '(-13.0 -4.0) :dead, '(0.0 -6.0) :alive, '(1.0 -6.0) :alive, '(5.0 -7.0) :dead, '(-6.0 -4.0) :alive, '(-3.0 -4.0) :alive, '(-14.0 -5.0) :dead, '(7.0 -5.0) :dead, '(-4.0 -3.0) :alive, '(4.0 -3.0) :alive, '(8.0 -6.0) :dead, '(9.0 -7.0) :dead, '(-11.0 -4.0) :dead, '(15.0 -6.0) :alive, '(0.0 -7.0) :alive, '(1.0 -7.0) :alive, '(-20.0 -4.0) :alive, '(-5.0 -2.0) :alive, '(-10.0 -4.0) :alive, '(-12.0 -5.0) :dead, '(12.0 -5.0) :dead, '(14.0 -6.0) :alive, '(-8.0 -7.0) :alive, '(-19.0 -4.0) :alive, '(-9.0 -2.0) :alive, '(11.0 -5.0) :dead, '(15.0 -7.0) :alive, '(-17.0 -4.0) :dead}]})

#_(run {:draw-fn brian-draw-fn :process-fn brian-process-fn :mouse-fn brian-mouse-fn
	:initial-configurations [ 
	{[10 -10] :alive, [-11 -10] :alive, '(-11 9) :dying, [-12 10] :dead, [11 10] :dying, [-12 -11] :dying, [11 -11] :dying, [-13 11] :dead, [-10 -10] :alive, [9 -10] :alive, [9 9] :dying, [-11 10] :alive, [10 10] :alive, [10 -11] :alive, [-11 -11] :alive, [-12 11] :dying, [-9 -10] :dying, [8 -10] :dying, [-10 10] :alive, [9 10] :alive, [9 -11] :alive, [-10 -11] :alive, [10 11] :alive, [-11 11] :alive, '(-9 10) :dying, [-10 -12] :dying, [9 -12] :dying, [9 11] :alive, '(-10 11) :alive, [10 12] :dying, [-11 12] :dead, [8 11] :dying, '(-10 12) :dying, [-11 -9] :dying, [10 -9] :dying, [-12 9] :dead}
	{'(-2.0 0.0) :alive, '(8.0 8.0) :alive, '(8.0 -8.0) :alive, '(9.0 9.0) :alive, '(9.0 -9.0) :alive, '(-2.0 1.0) :alive, '(-2.0 -1.0) :alive, '(8.0 9.0) :alive, '(8.0 -9.0) :alive, '(-3.0 -1.0) :alive, '(-3.0 1.0) :alive, '(-3.0 0.0) :alive, '(10.0 8.0) :alive, '(10.0 -8.0) :alive, '(9.0 8.0) :alive, '(9.0 -8.0) :alive, '(10.0 9.0) :alive, '(10.0 -9.0) :alive}
	{'(1.0 0.0) :dying, '(-3.0 0.0) :dead, '(-2.0 0.0) :dying, '(0.0 0.0) :alive, '(-1.0 0.0) :alive}
	{'(0.0 2.0) :alive, '(0.0 -2.0) :alive, '(-1.0 2.0) :alive, '(-4.0 2.0) :alive, '(0.0 1.0) :alive, '(-5.0 -3.0) :alive, '(0.0 -3.0) :alive, '(-1.0 -3.0) :alive, '(-4.0 -3.0) :alive, '(-5.0 1.0) :alive, '(-5.0 -2.0) :alive, '(-5.0 2.0) :alive}
	]})


#_(run {:draw-fn ant-draw-fn :process-fn ant-process-fn :mouse-fn ant-mouse-fn 
	:initial-configurations [{}]})
