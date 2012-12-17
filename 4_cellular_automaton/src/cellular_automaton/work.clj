(ns cellular-automaton.work
  (:use quil.core))

(def w 1200)
(def h 800)
(def dirs #{[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]})

(defmacro fn-state [vars & body]
  `(fn [] (let ~(vec (apply concat
                            (for [var vars]
                              [var `(state ~(keyword var))])))
            ~@body)))

(defn setup [{:keys [draw-fn process-fn mouse-fn initial-configurations]}]               
  (frame-rate 5)
  (fill 200)
  (def ant (load-image "ant.png")) 
  (set-state! 
  	:cells (atom (rand-nth initial-configurations))
  	:draw-fn draw-fn
  	:process-fn process-fn
  	:mouse-fn mouse-fn
  	:cell-size (atom 30)
  	:center-shift (atom (->> [w h] (map #(quot % -2)) (map #(quot % 30)) (map #(* 30 %)))) ;center shift always divisible by cell-size
  	:initial-configurations initial-configurations
  	:stop-update (atom true)))          

(defn to-real-coords [cell cell-size center-shift]
  (map - (map #(* @cell-size %) cell) @center-shift))

(defn from-real-coords [cell cell-size center-shift]
	(let [cell (map #(/ % @cell-size) (map + cell @center-shift))]
		(map #(Math/floor %) cell)))

(defn draw-cell [draw-fn cell cell-size center-shift]
  (let [[real-x real-y] (to-real-coords cell cell-size center-shift)]
    (draw-fn real-x real-y @cell-size @cell-size)))

(defn draw-ant [direction cell cell-size center-shift] 
 	(push-matrix)
	(let [[real-x real-y] (to-real-coords cell cell-size center-shift)]
		(translate (+ real-x (/ @cell-size 2)) (+ real-y (/ @cell-size 2))) ;translate at center of image to rotate
		(condp = direction
			:up 	 (rotate (radians 90))
			:right	(rotate (radians 180))
			:down 	(rotate (radians 270))
			:left 	 (rotate (radians 0)));  symmetry!
		(image ant (/ @cell-size -2) (/ @cell-size -2) @cell-size @cell-size))
	(pop-matrix))
                                
(def draw
  (fn-state [cells draw-fn cell-size center-shift]          
  	(background 0)
  	(stroke 0 0 0)
  	(stroke-weight 0)
  	(doall (map #(draw-fn % cell-size center-shift) @cells))))

(defn not-dead-neightbors [coord cells]
	(let [neightbors (map (fn [dir] (map + dir coord)) dirs)
		  neightbors-values (map #(do [(map - % coord) (get cells % :dead)]) neightbors)]
		  (filter #(not= % :dead) neightbors-values)))  

(def update
	(fn-state [cells process-fn stop-update]
		(when-not @stop-update
			(let [interesting-points (set (mapcat #(map (fn [dir] (map + dir %)) dirs) (keys @cells)))
				  interesting-map (merge (zipmap interesting-points (repeat :dead)) @cells)
				  new-cells-with-dead (into {} (map #(assoc {} (vec (key %)) (process-fn (val %) (not-dead-neightbors (first %) @cells))) interesting-map))
				  new-cells (into {} (filter #(not= :dead (val %)) new-cells-with-dead))
				  ]  
			 	  (reset! cells new-cells)))))

(def mouse-pressed
	(fn-state [cells mouse-fn cell-size center-shift]
		(do (swap! cells #(update-in % [(from-real-coords [(mouse-x) (mouse-y)] cell-size center-shift)] mouse-fn)))))

(def space (keyword " "))
(def key-pressed
	(fn-state [stop-update cells initial-configurations cell-size center-shift]
		(condp = (key-as-keyword)
			space (swap! stop-update not)
			:c (reset! cells {})
			:p (println (clojure.string/replace (str @cells) "(" "'("))
			:r (reset! cells (rand-nth initial-configurations))
			:q (do (swap! cell-size #(max 2 (dec %))) (reset! center-shift (->> [w h] (map #(quot % -2)) (map #(quot % @cell-size)) (map #(* @cell-size %)))))
			:e (do (swap! cell-size inc) (reset! center-shift (->> [w h] (map #(quot % -2)) (map #(quot % @cell-size)) (map #(* @cell-size %)))))
			(key-as-keyword) nil)))

(defn run [user-functions]
	(defsketch cellular-automaton                 
 	 :title "The title"  
 	 :setup #(setup user-functions)                     
 	 :mouse-pressed mouse-pressed
 	 :key-pressed key-pressed
 	 :draw #(do (update) (draw))                        
 	 :size [w h]))                  

;;; Your task is to implement cellular automaton.
;;; The most famous example of cellular automaton is Conway's Game of Life.
;;; Unlike previous tasks now you have to implement visualization and bots. So you need to implement everything :)
;;; I suggest to use quil library for animation (it was used in all previous tasks): https://github.com/quil/quil
;;; But of course you can use whatever you want.
;;; Keep in mind that is should be simple to run your simulator with different automata (Game of Life is only 1 example).






;;; Add ability to change cells' states by mouse click, to restart and pause simulation.