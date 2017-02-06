;;;; caro-busq.lisp

(in-package #:caro-busq)

;;; "caro-busq" goes here. Hacks and glory await!

 ;;; -------------------------------------------------------------------------
;;; ***************
;;; * TIC TAC TOE *
;;; ***************
;;;
;;;  Lisp Game Project
;;;
;;;  Team: Nguyen Hoang Thien Phuc
;;;        Nguyen Cong Thanh
;;;        Nguyen Truong Thinh
;;;
;;;  January 2017 - LINF14 - PUF HCM
;;; -------------------------------------------------------------------------

;;; Lispbuider Library call
(ql:quickload :lispbuilder-sdl)

;;; Variables
(defparameter *board-size* 10)
(defparameter *board* nil)

(defparameter *pressed-color* (sdl:color :r 150 :g 150 :b 150))
(defparameter *board-color* (sdl:color :r 200 :g 200 :b 200))

(defparameter *block* 0)
(defparameter *player* 1)
(defparameter *ai* 2)

;;; Functions for game

;;; Utils
(defmacro swap (var1 var2)
  "Swap 2 values"
  `(let ((tmp ,var1))
    (setq ,var1 ,var2)
    (setq ,var2 tmp)))

;;; Make Board
(defun make-board ()
  "Create the game board"
  (make-array '(*board-size* *board-size*)
	      :initial-element *block*))

(defun is-board-full ()
  
  )

(defun get-cell (board x y)
  (aref board x y))

(defun set-cell (board x y cell)
  "Update the game board"
  (setf (aref board x y) cell))

(defun is-cell-available (board x y)
  (eq (get-cell board x y) *block*))
  
(defun main-loop()
  (let ((board (make-board)))  
    ;; Clear display each game loop
    (sdl:clear-display sdl:*white*)

    (draw-board board)
    
     ;; Redraw display
    (sdl:update-display)))
  
;;; Mouse Events
(defun mouse-click ()
  "Set the mark when block is clicked"
  (when (sdl:mouse-left-p)
    (setf *board-color* *pressed-color*)))

;;; Drawing
(defun draw-cell (x y)
  (sdl:draw-box (sdl:rectangle-from-midpoint-* x y 20 20)
		:color *board-color*))


(defun draw-board (board x y)
  (mapcar (draw-cell x y) board))

(defun draw-mark ()
  (sdl:draw-box (sdl:rectangle-from-midpoint-* (sdl:mouse-x) (sdl:mouse-y) 20 20)
		:color sdl:*red*))

(defun draw-string ())
  
;;; Game Loop 
(defun start ()
  "Game start"
  (sdl:with-init (sdl:sdl-init-audio)
    (sdl:window 300 300
		:title-caption "Tic Tac Toe"
		:icon-caption "Tic Tac Toe")
    
    (setf (sdl:frame-rate) 60)    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event () (sdl:push-quit-event))
      (:idle ()
	     (main-loop)))))

(defun tic-tac-toe ()
  "Call this function for start the game"
  (start))
