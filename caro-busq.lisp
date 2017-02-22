;;;; Must -do
;;;; (ql:quickload '("lispbuilder-sdl" "lispbuilder-sdl-ttf" "lispbuilder-sdl-gfx"))

;;;; Root Params
(defparameter *root-path* "caro-busq")
(defparameter *gfx-path* (merge-pathnames "gfx/" *root-path*))

;;;; GFX
(defparameter *gfx-background* (merge-pathnames "background.png" *gfx-path*))
(defparameter *gfx-X* (merge-pathnames "X.png" *gfx-path*))
(defparameter *gfx-O* (merge-pathnames "O.png" *gfx-path*))

;;;; Game Params-
(defparameter *cell-size* 50)
(defparameter *cell-border* (floor *cell-size* 10))

(defparameter *board-rows* 5)
(defparameter *board-cols* 5)

(defparameter *game-width*
  (+ (* *cell-size* *board-cols*) (* *cell-border* (1+ *board-cols*))))

(defparameter *game-height*
  (+ (* *cell-size* *board-rows*) (* *cell-border* (1+ *board-rows*))))

(defparameter *board* nil)
(defparameter *current-player* nil)

(defparameter *win-number* 3)

;;;; Draw image

(defun draw-background (x y)
  (sdl:draw-surface-at-*
   (sdl:load-image *gfx-background* :color-key sdl:*white*) x y))

(defun draw-player (player x y)
  (sdl:draw-surface-at-*
   (sdl:load-image player :color-key sdl:*white*) x y))

;;;; Draw board

(defun draw-board (rows cols)
  "Draw the gameboard with number of rows and cols. Here we use
   draw-box method and recognize the position of a cell (row/col)
   by its coordinates. Then we check the value of each cell then
   draw the X/O/nothing box."
  (loop for row from 0 to (1- rows) do
       (loop for col from 0 to (1- cols) do
	    (let ((x (first (get-cell-coordinate row col)))
		  (y (second (get-cell-coordinate row col))))
	      (draw-background x y)
	      (case (get-cell-value row col)
		(X (draw-player *gfx-X* x y))
		(O (draw-player *gfx-O* x y)))))))

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
  (let ((cell (+ *cell-size* *cell-border*)))
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
    (format t "pos: ~D ~D ~%" row col)
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

(defun is-winner (x y)
  "Check if current player is win or not by checking the straight
   lines and diagonals."
  (or (is-straight-enough x y 0 1) ;; row
      (is-straight-enough x y 1 0) ;; col
      (is-straight-enough x y 1 1) ;; top left - bottom right
      (is-straight-enough x y -1 1))) ;; top right - bottom left 
			  

(defun is-straight-enough (x y dx dy)
  "Does player have enough marks in a straight line
   from position (x y) in direction (+/-dx +/-dy)?"
  (= (1+ *win-number*) (+ (count-marks-straight x y (- dx) (- dy))
			  (count-marks-straight x y dx dy))))

(defun count-marks-straight (x y dx dy)
  "Count player's marks from position (x y) in direction (dx dy)."
  (if (and (< -1 x *board-rows*)
	   (< -1 y *board-cols*)
	   (eq (get-cell-value x y) *current-player*))
      (1+ (count-marks-straight (+ x dx) (+ y dy) dx dy))
      0))			     

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
      (format t "in: ~D ~D ~%" row col)
      (cond
;;	((is-winner row col)
;;	 (format t "~A wins! ~%" *current-player*)
;;	 (reset-game))
	((is-full-board *board-rows* *board-cols*)
	 (format t "Tie! ~%")
	 (reset-game)))
      (toggle-player))))

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

(defun add-child (tree child)
  "Takes two nodes created with 'make-tree' and adds the
  second node as a child of the first. Returns the first node,
  which will be modified."
  (nconc (car tree) child)
  tree)

(defun is-leaf (tree)
  "Check if node is leaf"
  (listp (first-child tree)))

(defun make-tree (data)
  "Creates a new node that contains 'data' as its data."
  (cons (cons data nil) nil))

(defun count-leaves (tree)
  "Count children of node"
  (cond ((is-leaf tree) 1)
	(t (count-leaves-in-forest (first-child tree)))))

(defun count-leaves-in-forest (forest)
  "Count children in a node of children"
  (if (null forest)
      0
      (+ (count-leaves (first-child forest))
	 (count-leaves-in-forest (next-sibling forest)))))

;;;; AI

(defun alphabeta (node depth maximizing-player)
  (alphabeta-aux node depth -99999 99999 maximizing-player))
  
(defun alphabeta-aux (node depth alpha beta maximizing-player)
  node depth alpha beta maximizing-player)


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
