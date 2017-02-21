;;;; Must -do
;;;; (ql:quickload '("lispbuilder-sdl" "lispbuilder-sdl-ttf" "lispbuilder-sdl-gfx"))

;;;; Root Params
(defparameter *data-root* "caro-busq/")
(defparameter *audio-root* (merge-pathnames "audio/" *data-root*))
(defparameter *font-root* (merge-pathnames "fonts/" *data-root*))
(defparameter *gfx-root* (merge-pathnames "gfx/" *data-root*))

;;;; Game Params
(defparameter *cell-size* 50)
(defparameter *cell-border* (floor (/ *cell-size* 10)))
(defparameter *cell-center* (floor (/ *cell-size* 2)))

(defparameter *radius* (floor (* *cell-center* 0.95)))

(defparameter *board-rows* 5)
(defparameter *board-cols* 5)

(defparameter *game-width*
  (+ (* *cell-size* *board-cols*) (* *cell-border* (1+ *board-cols*))))

(defparameter *game-height*
  (+ (* *cell-size* *board-rows*) (* *cell-border* (1+ *board-rows*))))

(defparameter *game-state* 0) ; 0:menu/intro, 1:in game, 

(defparameter *board* nil)
(defparameter *current-player* nil)

(defparameter *win-number* 3)

;;;; Drawing  

(defun point (x y)
  "Draw a point with coordinates x y. Return vector."
  (sdl:point :x x :y y))

(defun draw-line (x1 y1 x2 y2)
  "Draw a line from top left corner with start coordinates x1 y1 
   and end coordinates x2 y2."
  (sdl:draw-line (point x1 y1) (point x2 y2) :color sdl:*red*))

(defun draw-cross (x y)
  "Draw a cross from top left corner with coordinates x y."
  (let ((x1 (+ x *cell-border*))
	(x2 (- x *cell-border*))
	(y1 (+ y *cell-border*))
	(y2 (- y *cell-border*)))
    (draw-line x1 y1 (+ x2 *cell-size*) (+ y2 *cell-size*))
    (draw-line (+ x2 *cell-size*) y1 x1 (+ y2 *cell-size*))))

(defun draw-circle (x y)
  "Draw cirle from center with coordinat1es x y."
  (sdl:draw-circle (point (+ x *cell-center*) (+ y *cell-center*))
		   *radius*
		   :color sdl:*blue*))			      

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
  (if (eq *current-player* 'X) 
      (setf *current-player* 'O)
      (setf *current-player* 'X)))

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
  (let ((cell (+ *cell-size* (* 2 *cell-border*))))
    (list (floor x cell)
	  (floor y cell))))

;;;; Game Flow

(defun reset-game ()
  "Initilize game."
  (setf *current-player* nil)
  (setf *board* (make-board)))  

(defun is-cell-clicked (row col)
  "Check if the cell is clicked. If not, set value for cell. Else,
   return true."
  (unless (get-cell-value row col)
    (set-cell-value *current-player* row col)
    t))

;;;; Check winner

(defun is-full-board (rows cols)
  "Check if the board is full or not. If not return nil. Else true."
  (loop for row from 0 to (1- rows) do
       (loop for col from 0 to (1- cols) do
	    (unless (get-cell-value col row)
	      (return-from is-full-board nil))))
  t)

(defun is-winner (rows cols)
  "Check if current player is win or not by checking the straight
   lines and diagonals."
  (or (is-diagonals-enough rows)
      (is-lines-enough rows cols)))

(defun is-lines-enough (rows cols)
  "Check straight lines full"
  (loop for i from 0 to (1- rows) thereis
     (loop for j from 0 to (1- cols) thereis
	  (or (is-rows-enough i j)
	      (is-cols-enough i j)))))

(defun is-diagonals-enough (rows)
  "Check diagonals lines full"
  (loop for i from 0 to (1- rows) thereis
       (or (is-topleft-bottomright-enough i)
	   (is-bottomleft-topright-enough i))))
      
(defun is-rows-enough (i j)
  "Check rows straight"
  (let ((end (+ j (1- *win-number*))))
    (loop for col from j to end always
	 (and (< end *board-cols*)
	      (eq (get-cell-value col i) *current-player*)))))

(defun is-cols-enough (i j)
  "Check cols straight"
  (let ((end (+ i (1- *win-number*))))
    (loop for row from i to end always
	 (and (< end *board-rows*)
	      (eq (get-cell-value j row) *current-player*)))))

(defun is-topleft-bottomright-enough (i)
  "Check top left - bottom right straight"
  (let ((end (+ i (1- *win-number*))))
    (loop for j from i to end always
	 (and (< end *board-rows*)
	      (< end *board-cols*)
	      (eq (get-cell-value j j) *current-player*)))))

(defun is-bottomleft-topright-enough (i)
  "Check bottom left - top right straight"
  (let ((end (+ i (1- *win-number*))))
    (loop for j from i to end always
	 (and (< end *board-rows*)
	      (< end *board-cols*)
	      (> end 0)
	      (eq (get-cell-value (- end j) j) *current-player*)))))

;;;; Tree Building

(defun first-child (tree)
  "Returns a reference to the first child of the node passed in,
  or nil if this node does not have children."
  (if (null tree)
      nil
      (cdar tree)))

(defun next-sibling (tree)
  "Returns a reference to the next sibling of the node passed in,
  or nil if this node does not have any siblings."
  (cdr tree))

(defun data (tree)
  "Returns the information contained in this node."
  (caar tree))

(defun make-tree (data)
  "Creates a new node that contains 'data' as its data."
  (cons (cons data nil) nil))

(defun add-child (tree child)
  "Takes two nodes created with 'make-tree' and adds the
  second node as a child of the first. Returns the first node,
  which will be modified."
  (nconc (car tree) child)
  tree)

(defun is-leaf (tree)
  (listp (first-child tree)))

(defun count-leaves (tree)
  (cond ((is-leaf tree) 1)
	(t (count-leaves-in-forest (first-child tree)))))

(defun count-leaves-in-forest (forest)
  (if (null forest)
      0
      (+ (count-leaves (first-child forest))
	 (count-leaves-in-forest (next-sibling forest)))))

;;;; AI

(defun min-max (tree)
  (let ((val (data tree)))
    (if (not (null tree))
	(setf val (min-max-aux val tree *current-player*)))))

(defun min-max-aux (val tree player)
  (let ((info (data tree)))
    ;; if not leaf  
    (cond ((is-leaf tree) (setf val info))
	  ;; Traverse children
	  (t (loop for i from 0 to (1- (count-leaves tree)) do
		  (if (eq player 'X)
		      (progn
			(setf info
			      (max info
				   (min-max-aux info
						(list (nth i (first-child tree))) 'O)))
			(setf val info))
		      (progn
			(setf info
			      (min info
				   (min-max-aux info
						(list (nth i (first-child tree))) 'O)))
			
			(setf val info))))))))

	      
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
    (when (is-cell-clicked row col)
      (cond
	((is-winner *board-rows* *board-cols*)
	 (format t "~a wins! ~%" *current-player*)
	 (reset-game))
	((is-full-board *board-rows* *board-cols*)
	 (format t "Tie! ~%")
	 (reset-game)))
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
