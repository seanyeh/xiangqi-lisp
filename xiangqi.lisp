;;; xiangqi.lisp
;;;
;;; Copyright (c) 2012 Sean Yeh
;;;
;;; This file is licensed under the MIT license; 
;;; see the LICENSE file for details

; My own concatenate function
(defun s+ (&rest args)
  (reduce (lambda (x y)
            (let ((result (cond
                            ((numberp y) (write-to-string y))
                            ((characterp y) (string y))
                            (T y))))
              (concatenate 'string x result)))
          (append '("") args)))

;; PLAYER
(defconstant REDPLAYERSTART
             (let ((side "RED"))
               (append (setup-pieces SOLDIER side "3" SOLDIER_COLS)
                       (setup-pieces CANNON side "2" CANNON_COLS)
                       (setup-pieces ROOK side "0" ROOK_COLS)
                       (setup-pieces HORSE side "0" HORSE_COLS)
                       (setup-pieces ELEPHANT side "0" ELEPHANT_COLS)
                       (setup-pieces ADVISOR side "0" ADVISOR_COLS)
                       (setup-pieces GENERAL side "0" GENERAL_COLS))))
(defconstant BLACKPLAYERSTART
             (let ((side "BLACK"))
               (append (setup-pieces SOLDIER side "6" SOLDIER_COLS)
                       (setup-pieces CANNON side "7" CANNON_COLS)
                       (setup-pieces ROOK side "9" ROOK_COLS)
                       (setup-pieces HORSE side "9" HORSE_COLS)
                       (setup-pieces ELEPHANT side "9" ELEPHANT_COLS)
                       (setup-pieces ADVISOR side "9" ADVISOR_COLS)
                       (setup-pieces GENERAL side "9" GENERAL_COLS))))

; Player struct
(defstruct player
  pieces
  name)

(defun get-alive-pieces(board player)
  (let ((pieces (player-pieces player)))
    (mapcan
      (lambda (piece)
        (and (equal piece (get-piece board (piece-pos piece)))
             (list piece)))
      pieces
      )))

(defun remove-piece(player piece)
  (setf (player-pieces player) (remove piece (player-pieces player))))

(defun switch-player(current-player red-player black-player)
  (let ((side (player-name current-player)))
    (cond
      ((string= side "RED") black-player)
      ((string= side "BLACK") red-player)
      (T nil))))

(defun get-valid-moves (board player other-player)
  (mapcan (lambda(move)
            (let* ((piece (car move))
                   (old-pos (cadr move))
                   (new-pos (caddr move))
                   (is-valid (board-is-valid
                               (move-piece (copy-board board) piece old-pos new-pos) player other-player)))
              ;revert to piece to original pos
              (setf (piece-pos piece) old-pos)
              (and is-valid (list move))))
          (get-all-moves board player)))

(defun get-all-moves (board player)
  (apply #'append 
         (mapcar (lambda (piece) (piece-get-all-moves board piece)) 
                 (get-alive-pieces board player))))


;; PIECES

(defstruct piece
  name
  pos
  side ;"RED" or "BLACK"
  move-spaces
  (move-limits #'no-move-limits)
  eat-spaces)

; Returns ((piece1 oldpos1 newpos1) (piece2 oldpos2 newpos2)...)
(defun piece-get-all-moves(board piece)
  (mapcar (lambda (possible-pos)
            (list piece (piece-pos piece) possible-pos))
          (append (get-eat-spaces board piece) (get-strict-move-spaces board piece))))


; move piece, return board
(defun move-piece(board piece old-pos new-pos &optional(actual T))
  (if actual
    (setf (piece-pos piece) new-pos))
  ; delete old position from board
  (set-space board old-pos)

  ; put in new position on board
  (set-space board new-pos piece)
  board)

(defun same-piece(p1 p2) 
  (cond ((stringp p2) nil)
        (T (string= (to-string p1) (to-string p2)))))

(defun try-move-piece (board old-pos new-pos player other-player)
  (let ((piece (get-piece board old-pos))
        (replaced-piece (get-piece board new-pos))
        (side (player-name player)))
    (print (s+ (player-name other-player) " possible moves: "
                        (length (get-valid-moves board player other-player))))
    (cond
      ; probably is some blank string like _
      ((stringp piece) nil)
      ; if wrong side, nil
      ((not (string= side (piece-side piece))) 
       (progn (print "WRONG SIDE") nil))
      ((or 
         ; If eat
         (member new-pos (get-eat-spaces board piece) :test #'equal)
         ; If possible move space, space is empty
         (and (member new-pos (get-move-spaces board piece) :test #'equal)
              (is-empty-space board new-pos)))
       (if (board-is-valid 
             (move-piece (copy-board board) piece old-pos new-pos) player other-player)
         (move-piece board piece old-pos new-pos)
         ; Revert
         (progn (setf (piece-pos piece) old-pos) 
                nil)
         ))
      (T nil))))


; like get-move-spaces, but does not include any moves that involve eats
(defun get-strict-move-spaces (board piece)
  (mapcan (lambda(pos)
            (and (is-empty-space board pos) (list pos)))
          (get-move-spaces board piece)))

(defun get-move-spaces (board piece)
  (let* ((pos (piece-pos piece))
         (side (piece-side piece)) 
         (possible-move-spaces (funcall (piece-move-spaces piece) board pos side))
         (move-limits (piece-move-limits piece)))
    (mapcan
      (lambda (x)
        (and x (funcall move-limits board x) (list x)))
      possible-move-spaces
      )
    ))

(defun get-eat-spaces(board piece)
  ; Cannon is the only piece that eats differently than it moves
  (let* ((pos (piece-pos piece))
         (side (piece-side piece)) 
         (possible-eat-spaces 
           (if (string= (to-string piece) "C")
             (cannon-eat-spaces board (piece-pos piece))
             (get-move-spaces board piece))))
    (mapcan 
      (lambda(eat-pos)
        (let ((victim-piece (get-piece board eat-pos)))
          ; Is valid eat position if enemy piece
          (and eat-pos (not (stringp victim-piece))
               (not (string= (piece-side victim-piece) side))
               (list eat-pos))))
      possible-eat-spaces)
    ))

; MOVE SPACES FUNCTIONS
(defun general-move-spaces(board pos side)
  (list (move-n pos) (move-e pos) (move-s pos) (move-w pos)))

(defun rook-move-spaces(board pos side)
  (append (get-greedy-spaces board (move-n pos) #'move-n)
          (get-greedy-spaces board (move-e pos) #'move-e)
          (get-greedy-spaces board (move-s pos) #'move-s)
          (get-greedy-spaces board (move-w pos) #'move-w)))

(defun cannon-eat-spaces(board pos)
  (let ((npos (car (last (get-greedy-spaces board (move-n pos) #'move-n))))
        (epos (car (last (get-greedy-spaces board (move-e pos) #'move-e))))
        (spos (car (last (get-greedy-spaces board (move-s pos) #'move-s))))
        (wpos (car (last (get-greedy-spaces board (move-w pos) #'move-w)))))
    (append (get-greedy-spaces board (move-n npos) #'move-n)
            (get-greedy-spaces board (move-e epos) #'move-e)
            (get-greedy-spaces board (move-s spos) #'move-s)
            (get-greedy-spaces board (move-w wpos) #'move-w))))

(defun horse-move-spaces(board pos side)
  (mapcan
    (lambda(x)
      (let* (
             (temppos (funcall (car x) pos))
             (newpos1 (funcall (cadr x) temppos))
             (newpos2 (funcall (caddr x) temppos)))
        (and temppos (is-empty-space board temppos) 
             (mapcan (lambda(y) (and y (list y))) (list newpos1 newpos2)))
        ))
    '((move-n move-ne move-nw)
      (move-e move-ne move-se)
      (move-s move-se move-sw)
      (move-w move-nw move-sw))
    )
  )

(defun advisor-move-spaces(board pos side)
  (list (move-ne pos) (move-nw pos) (move-se pos) (move-sw pos)))

(defun elephant-move-spaces(board pos side)
  (mapcan
    (lambda(x)
      (let ((temppos (funcall x pos)))
        (and temppos (is-empty-space board temppos) (funcall x temppos) (list (funcall x temppos)))
        ))
    '(move-ne move-nw move-se move-sw))
  )

(defun soldier-move-spaces(board pos side)
  (cond
    ((null side) nil)
    ; if passed river
    ((and (equal side "RED") (char>= (elt pos 1) #\5))
     (list (move-n pos) (move-e pos) (move-w pos)))
    ((and (equal side "BLACK") (char<= (elt pos 1) #\4))
     (list (move-s pos) (move-e pos) (move-w pos)))
    ;not passed river
    ((equal side "RED") (list (move-n pos)))
    ((equal side "BLACK") (list (move-s pos)))
    ; if somehow side is not right nil
    (T nil)))

(defun no-move-limits(board pos) pos)

(defun palace-limits(board pos)
  (and pos (char>= (elt pos 0) #\d) (char<= (elt pos 0) #\f) 
       (or (char>= (elt pos 1) #\7) (char<= (elt pos 1) #\2))))

; easy way: row cannot be 3 or 6
(defun elephant-move-limits(board pos)
  (and pos 
       (not (or (equal (elt pos 1) #\3) 
                (equal (elt pos 1) #\6)))))

; To get spaces in a certain direction for pieces that move >=1 spaces
(defun get-greedy-spaces(board pos pos-change)
  (cond 
    ((null pos) nil)
    ((not (is-empty-space board pos)) (list pos))
    (T (append (list pos) 
               (get-greedy-spaces board (funcall pos-change pos) pos-change)))
    ))

(defun is-empty-space(board x)
  (equal (get-piece board x) "_"))

(defconstant ADVISOR (make-piece :name "A" :move-spaces #'advisor-move-spaces
                                 :move-limits #'palace-limits))
(defconstant CANNON (make-piece :name "C" :move-spaces #'rook-move-spaces))
(defconstant ROOK (make-piece :name "R" :move-spaces #'rook-move-spaces))
(defconstant ELEPHANT (make-piece :name "E" :move-spaces #'elephant-move-spaces
                                  :move-limits #'elephant-move-limits))
(defconstant GENERAL (make-piece :name "G" :move-spaces #'general-move-spaces 
                                 :move-limits #'palace-limits))
(defconstant HORSE (make-piece :name "H" :move-spaces #'horse-move-spaces))
(defconstant SOLDIER (make-piece :name "S" :move-spaces #'soldier-move-spaces 
                                 :pos "c3"))

(defun new-piece(piece-type side pos)
  (let ((piece (copy-piece piece-type)))
    (setf (piece-pos piece) pos)
    (setf (piece-side piece) side)
    piece
    ))

(defun move-n(pos) (move-x pos 0 1))
(defun move-e(pos) (move-x pos 1 0))
(defun move-s(pos) (move-x pos 0 -1))
(defun move-w(pos) (move-x pos -1 0))
(defun move-ne(pos) (move-x pos 1 1))
(defun move-nw(pos) (move-x pos -1 1))
(defun move-se(pos) (move-x pos 1 -1))
(defun move-sw(pos) (move-x pos -1 -1))

(defun move-x(pos addx addy)
  (if (null pos)
    nil ; return nil early if original pos is nil
    (let ( (x (ascii-inc (elt pos 0) addx)) (y (ascii-inc (elt pos 1) addy)) )
      (cond
        ((equal y #\:) nil)
        ((equal y #\/) nil)
        ((equal x #\`) nil)
        ((equal x #\j) nil)
        (T (s+ x y))
        ))
    ))

; Increment
(defun ascii-inc(x addx) (code-char (+ addx (char-code x))))

; Should be customizable, will implement later
(defun get-color-code (side)
  (cond
    ((string= side "BLACK") "[1;1m")
    ((string= side "RED") "[1;31m")
    ((string= side "DEFAULT") "[0;0m")))

(defun to-string(piece &optional(color nil))
  (cond
    ((stringp piece) piece)
    ((null piece) nil)
    (color (s+ #\Esc (get-color-code (piece-side piece))
                        (piece-name piece) 
                        #\Esc (get-color-code "DEFAULT")))
    (T (piece-name piece))
    ))

;; BOARD
(defun print-board(board &optional (row-num 9))
  (if (null board)
    (progn (print "  A B C D E F G H I") (print "") nil)
    (progn 
      (print 
        (s+ row-num " " (row-to-string (car board))))
      (print-board (cdr board) (1- row-num)))
    ))

(defun copy-board (board) (mapcar #'copy-list board))

; is board valid for player?
(defun board-is-valid(board player other-player)
  (let ((king (find GENERAL (player-pieces player) :test #'same-piece)))
    (cond
      ((king-face-king board king) (progn (print "KING FACE KING") nil))
      ((is-checking board king other-player) nil)
      (T T))
    ))


; magic numbers: 48 is char code of #\0, 97 #\a
(defun to-row-index(pos)
  (- 9 (- (char-code (elt pos 1)) 48)))
(defun to-col-index(pos)
  (- (char-code (elt pos 0)) 97))

(defun get-piece (board pos)
  (if (null pos)
    nil
    (nth (to-col-index pos) (nth (to-row-index pos) board))))

(defun set-space (board pos &optional(piece "_"))
  (setf (nth (to-col-index pos) (nth (to-row-index pos) board)) piece))

(defun is-checking(board king other-player)
  (member (piece-pos king)
          (mapcan (lambda(piece)
                    (append (get-eat-spaces board piece) '()))
                  (get-alive-pieces board other-player)) :test #'string=))

; return T if king faces king
(defun king-face-king(board king)
  (let* (
         (king-pos (piece-pos king))
         (n-spaces (get-greedy-spaces board (move-n king-pos) #'move-n))
         (s-spaces (get-greedy-spaces board (move-s king-pos) #'move-s)))
    (or
      (same-piece king (get-piece board (car (last n-spaces))))
      (same-piece king (get-piece board (car (last s-spaces)))))))

(defun row-to-string(row)
  (cond
    ((null row) nil)
    (T (s+ (to-string (car row) T) " " 
                    (row-to-string (cdr row))))
    ))

(defconstant STARTBOARD '(
                          ("_" "_" "_" "_" "_" "_" "_" "_" "_")
                          ("_" "_" "_" "_" "_" "_" "_" "_" "_")
                          ("_" "_" "_" "_" "_" "_" "_" "_" "_")
                          ("_" "_" "_" "_" "_" "_" "_" "_" "_")
                          ("_" "_" "_" "_" "_" "_" "_" "_" "_")
                          ("_" "_" "_" "_" "_" "_" "_" "_" "_")
                          ("_" "_" "_" "_" "_" "_" "_" "_" "_")
                          ("_" "_" "_" "_" "_" "_" "_" "_" "_")
                          ("_" "_" "_" "_" "_" "_" "_" "_" "_")
                          ("_" "_" "_" "_" "_" "_" "_" "_" "_")
                          ))

(defun setup-board(red black)
  (let ((board STARTBOARD))
    (mapcar
      (lambda (piece)
        (set-space board (piece-pos piece) piece))
      (append (player-pieces red) (player-pieces black))
      )
    board))

(defun setup-pieces(piece-type side row col-list)
  (mapcar (lambda(col)
            (new-piece piece-type side (s+ col row)))
          col-list)
  )

(defconstant SOLDIER_COLS '("a" "c" "e" "g" "i"))
(defconstant CANNON_COLS '("b" "h"))
(defconstant ROOK_COLS '("a" "i"))
(defconstant HORSE_COLS '("b" "h"))
(defconstant ELEPHANT_COLS '("c" "g"))
(defconstant ADVISOR_COLS '("d" "f"))
(defconstant GENERAL_COLS '("e"))

; a-i, 0-9
(defun is-valid-pos(pos)
  (let ((row (subseq pos 0 1))
        (col (subseq pos 1 2)))
    (and (string>= row #\a) (string<= row #\i)
         (string>= col #\0) (string<= col #\9))))

;Parse input and return move (like  (old-pos new-pos))
(defun move-from-input(input)
    (cond
      ((< (length input) 4) '(nil nil))
      (T (let ((pos1 (subseq input 0 2))
               (pos2 (subseq input 2)))
           (if (not (and (is-valid-pos pos1) (is-valid-pos pos2)))
             '(nil nil)
             (list pos1 pos2))))
      ))

;; Main
(defun play()
  (let* ((red-player (make-player :name "RED" :pieces REDPLAYERSTART))
         (black-player (make-player :name "BLACK" :pieces BLACKPLAYERSTART))
         (board (setup-board red-player black-player))
         (current-player red-player)
         (other-player black-player))
    (loop
      (print-board board)
      (print (s+ "alive pieces: "
                 (length (get-alive-pieces board current-player))))
      (print (s+ (player-name current-player) " turn: "))

      (when (not (board-is-valid board current-player other-player))
        ; Player is in check
        (let ((possible-moves 
                (get-valid-moves board current-player other-player)))
          (print (s+ (player-name current-player) " is in check"))
          (when (= 0 (length possible-moves)) 
            (progn (print (s+ "CHECKMATE! " (player-name other-player) "wins!")) 
                   (return)))
          ))
      (let* ((cur-move (move-from-input(read-line)))
             (pos1 (car cur-move))
             (pos2 (cadr cur-move)))
        (cond
          ((or (null pos1) (null pos2))
           (print "Invalid move"))
          ((try-move-piece board pos1 pos2 current-player other-player)
           (progn (setf other-player current-player)
                  (setf current-player 
                        (switch-player current-player red-player black-player)) 
                  (print "Valid")))
          (T (print "Not Valid"))
          ))
      )))

(provide "xiangqi")
(play)
