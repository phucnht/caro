;;;; Must -do
;;;; (ql:quickload '("lispbuilder-sdl" "lispbuilder-sdl-ttf" "lispbuilder-sdl-gfx"))

;;;; Root Params
(defparameter *data-root* "caro-busq/")
(defparameter *audio-root* (merge-pathnames "audio/" *data-root*))
(defparameter *font-root* (merge-pathnames "fonts/" *data-root*))
(defparameter *gfx-root* (merge-pathnames "gfx/" *data-root*))

;;;; Game Params
(defparameter *cell-size* 200)
(defparameter *cell-border* 10)
(defparameter *cell-center* (floor (/ *cell-size* 2)))

(defparameter *radius* (floor (* *cell-center* 0.95)))

(defparameter *board-rows* 3)
(defparameter *board-cols* 3)

(defparameter *game-width*
  (+ (* *cell-size* *board-cols*) (* *cell-border* (1+ *board-cols*))))

(defparameter *game-height*
  (+ (* *cell-size* *board-rows*) (* *cell-border* (1+ *board-rows*))))

(defparameter *game-state* 0) ; 0:menu/intro, 1:in game, 2:game over

(defparameter *board* nil)
(defparameter *current-player* nil)

;;;; Drawing  

(defun point (x y)
  "Draw a point with coordinates x y. Return vector."
  (sdl:point :x x :y y))

(defun draw-line (x1 y1 x2 y2)
  "Draw a line from top left corner with start coordinates x1 y1 
   and end coordinates x2 y2."
  (sdl:draw-line (point x1 y1) (point x2 y2) :color sdl:*black*))

(defun draw-cross (x y)
  "Draw a cross from top left corner with coordinates x y."
  (let ((x1 (+ x *cell-border*))
	(x2 (- x *cell-border*))
	(y1 (+ y *cell-border*))
	(y2 (- y *cell-border*)))
    (draw-line x1 y1 (+ x2 *cell-size*) (+ y2 *cell-size*))
    (draw-line (+ x2 *cell-size*) y1 x1 (+ y2 *cell-size*))))

(defun draw-circle (x y)
  "Draw cirle from center with coordinates x y."
  (sdl:draw-circle (point (+ x *cell-center*) (+ y *cell-center*))
		   *radius*
		   :color sdl:*black*))			      

(defun draw-rectangle (x y h w)
  "Draw rectangle from top left corner with coordinates x y 
   and height h and weight w."
  (sdl:rectangle :x x :y y :w w :h h))

(defun draw-box (rect)
  "Draw a filled rectangle rect."
  (sdl:draw-box rect :color sdl:*white*))

(defun draw-board (rows cols)
  "Draw the gameboard with number of rows and cols. Here we use
   draw-box method and recognize the position of a cell (row/col)
   by its coordinates. Then we check the value of each cell then
   draw the X/O/nothing box."
  (loop for row from 0 to (1- rows) do
       (loop for col from 0 to (1- cols) do
	    (let ((x (first (get-cell-coordinate row col)))
		  (y (second (get-cell-coordinate row col))))
	      (draw-box (draw-rectangle x y *cell-size* *cell-size*))
	      (case (get-cell-value row col)
		(X (draw-cross x y))
		(O (draw-circle x y)))))))

;;;; Player

(defun toggle-player ()
  "Switch player X/O. Begin game with X player."
  (case *current-player*
    (X (setf *current-player* 'O))
    (O (setf *current-player* 'X))
    (otherwise (setf *current-player* 'X))))

;;;; Board

(defun make-board ()
  "Initialize a array with rows/cols specified filled by nil element."
  (make-array `(,*board-rows* ,*board-cols*)
	      :initial-element nil))

;;;; Cell

(defun get-cell-value (row col)
  "Get the value of cell in position row/col. The reverse of values 
   here is caused by the difference between the references of graphic 
   and environment"
  (aref *board* col row))

(defun set-cell-value (symbol row col)
  "Set the cell in position row/col with the value symbol."
  (setf (aref *board* col row) symbol))

(defun get-cell-coordinate (row col)
  "Get list of cell's coordinates from position row/col. Cells 
   are separated by borders."
  (list (+ (* *cell-size* row) (* *cell-border* (1+ row)))
	(+ (* *cell-size* col) (* *cell-border* (1+ col)))))

(defun get-cell-pos (x y)
  "Get list of cell's position row/col from coordinates x/y)"
  (list (floor x *cell-size*)
	(floor y *cell-size*)))

;;;; Game Flow

(defun reset-game ()
  "Initilize game."
  (setf *current-player* nil)
  (setf *board* (make-board)))  

(defun cell-clicked (row col)
  "Check if the cell is clicked. (Different nil)"
  (unless (get-cell-value row col)
    (set-cell-value *current-player* row col)
    t))
    
;;;; Events & Rendering

(defun render ()
  "Update the game by redrawing game board."
  (sdl:clear-display sdl:*black*)
  (draw-board *board-rows* *board-cols*)
  (sdl:update-display))

(defun perform-turn (x y)
  "Realize a turn by click to the cell then switch player."
  (let ((row (first (get-cell-pos x y)))
	(col (second (get-cell-pos x y))))
    (when (cell-clicked row col)
      (toggle-player))))

;;;; Main

(defun main ()
  "Begin game. Set player."
  (reset-game)
  (toggle-player)
  (sdl:with-init ()
    (sdl:window *game-width* *game-height*
		:title-caption "Caro Busq"
		:icon-caption "Caro Busq")
    
    (sdl:with-events ()
      (:quit-event () t)

      (:video-expose-event () (sdl:update-display))
      
      (:key-down-event () (sdl:push-quit-event))
      
      (:mouse-button-down-event (:x x :y y) (perform-turn x y))
      
      (:idle () (render)))))
