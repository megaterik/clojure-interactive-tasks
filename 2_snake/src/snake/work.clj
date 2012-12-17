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
(defn rng-snake [[snake-x snake-y] [apple-x apple-y]]
	(cond 
		(> snake-x apple-x) :left
		(< snake-x apple-x) :right
		(> snake-y apple-y) :up
		:else			    :down
	)
)

(defn distance-y-to-apple [y1 y2]
	(if (<= y1 y2)
		(- y2 y1)
		(- (+ y2 30) y1)))


(defn snake-that-has-plenty-of-spare-time-and-lives-by-the-proverb-slow-but-sure [snake [apple-x apple-y]]
	(let  [safety-time (+ 1 (/ (count snake) 40))
		   [snake-x snake-y] (first snake)
		   [body-x body-y] (nth snake 1 [0 0])
		   can-right (and (>= snake-x body-x) (< snake-x 39))
		   can-left (and (<= snake-x body-x) (> snake-x 0))]
		(cond
			(= 1 (count snake)) (rng-snake (first snake) [apple-x apple-y])
			(> (distance-y-to-apple snake-y apple-y) safety-time) :down
			(and can-right (< snake-x apple-x)) :right
			(and can-left  (> snake-x apple-x)) :left
			can-right :right
			can-left :left
			:else :down
		)))

;(run-not-grow rng-snake)



;;; Snake grows now (each time snake eats an apple, it's length increases).
;;; You need to write similar function as in previous task.
;;; It takes 2 arguments.
;;; First argument is snake's body - collection of cells, each cell is a vector of x and y. First cell is snake's head.
;;; Second argument is apple's position - vector of x and y.
;;; It should return direction: :up, :down, :left or :right.
;;; Note that you cannot change direction to the opposite in 1 move: snake will hit it's tail if length is 2 or more.
;;; Wait, you can change direction but snake will die :\

;;; Uncomment and substitute your solution
;(run-grow snake-that-has-plenty-of-spare-time-and-lives-by-the-proverb-slow-but-sure)



;;; Now you have many apples (5) instead of one.
;;; Function the same as previous but it takes set of apples instead of the single apple.
;;; Each apple in the set is a vector of x and y.
;;; E.g. you can try to reach nearest apple to the snake.

;;; Uncomment and substitute your solution

(defn distance-to-apple [[[snake-x snake-y] _] [apple-x apple-y]]
	(if (<= snake-y apple-y) (- apple-y snake-y) (- (+ apple-y 30) snake-y)))

(defn rma-snake [snake apples]
	(snake-that-has-plenty-of-spare-time-and-lives-by-the-proverb-slow-but-sure 
		snake 
		(reduce #(if (< (distance-to-apple snake %) (distance-to-apple snake %2)) % %2) apples)))
(run-many-apples rma-snake)



;;; Walls are added. So snake can hit wall and die.
;;; Your function now takes third argument - set of walls.
;;; Each wall is a cell that snake is not allowed to  move to.
;;; Wall is a vector of x and y.

#_(def previous-head (atom [0 0]))
#_(defn snake-that-has-plenty-of-spare-time-and-lives-by-the-proverb-slow-but-sure-with-walls [snake [apple-x apple-y] walls]
(let  [    walls (convert-to-squared-walls walls)
		   safety-time (+ 1 (/ (count snake) 40))
		   [snake-x snake-y] (first snake)
		   [body-x body-y] (nth snake 1 @previous-head)
		   can-turn-later-right (not= (- 40 snake-x) (count (filter #(and (< snake-x (first %)) (= snake-y (second %))) walls)))
		   can-turn-later-left  (not= snake-x (count (filter #(and (> snake-x (first %)) (= snake-y (second %))) walls)))
		   can-right (and (>= snake-x body-x) (< snake-x 39) (not (walls [(inc snake-x) snake-y])) can-turn-later-right)
		   can-left (and (<= snake-x body-x) (> snake-x 0) (not (walls [(dec snake-x) snake-y])) can-turn-later-left)
		   can-right-if-urgent (and (>= snake-x body-x) (not (walls [(inc snake-x) snake-y])) can-turn-later-right)
		   can-left-if-urgent (and (<= snake-x body-x) (not (walls [(dec snake-x) snake-y])) can-turn-later-left)
		   can-down (not (walls [snake-x (inc snake-y)]))]
		   (reset! previous-head [snake-x snake-y])
		(cond
			(and (> (distance-y-to-apple snake-y apple-y) safety-time) can-down) :down
			(and can-right (< snake-x apple-x)) :right
			(and can-left  (> snake-x apple-x)) :left
			can-right :right
			can-left :left
			can-down :down
			can-right-if-urgent :right
			can-left-if-urgent :left
			:else :right
		)))

;;; Uncomment and substitute your solution
#_(defn rww-snake [snake apples walls]
	(snake-that-has-plenty-of-spare-time-and-lives-by-the-proverb-slow-but-sure-with-walls 
		snake
		(reduce #(if (< (distance-to-apple snake %) (distance-to-apple snake %2)) % %2) apples) walls))

;coming soon!
;(run-with-walls rww-snake)