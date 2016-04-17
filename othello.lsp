(load (merge-pathnames "input.lsp" *load-truename*))
(load (merge-pathnames "othello-init.lsp" *load-truename*))
(load (merge-pathnames "minimax.lsp" *load-truename*))
(load (merge-pathnames "move-generator.lsp" *load-truename*))
(load (merge-pathnames "heuristics.lsp" *load-truename*))

(defvar *positionalStrat* '(99 -8 8 6 6 8 -8 99 -8 -24 -4 -3 -3 -4 -24 -8 8 -4 7 4 4 7 -4 8 6 -3 4 0 0 4 -3 6 6 -3 4 0 0 4 -3 6 8 -4 7 4 4 7 -4 8 -8 -24 -4 -3 -3 -4 -24 -8 99 -8 8 6 6 8 -8 99))

(defvar *edgeTopRow*      '(1 2 3 4 5 6 ))
(defvar *edgeLeftColumn* '(8 16 24 32 40 48 ))
(defvar *edgeBottomRow*   '(57 58 59 60 61 62 ))
(defvar *edgeRightColumn*  '(55 47 39 31 23 15))

(defun othello (args)
	(input args)
	(if 
		(eq *player* 'w)
		playerFirst
	)
		
	endgame
)


(defun playerFirst
	(let (temp)	
	(do ( ( i 0 (1+ i) ) )

		(( >= i 10) ‘done)  							;termination test
		(format t "Please enter the row, column coordinates of your move:")
		(setf temp (read))
		(CreateMove (lst pos endPos endColor))
		(move-generator 'w)
	)
))


(defun opponentFirst
	(let (temp)	
	(do ( ( i 0 (1+ i) ) )
       	
		(( >= i 10) ‘done)  							;termination test
		(move-generator 'b)
		(format t "Please enter the row, column coordinates of your move:")
		(setf temp (read))
		(CreateMove (lst pos endPos endColor))

	)
))


(defun endgame (board)
	(let (temp)
	(format t "Score: ")
	(format t "Would you like to move first [y/n]? ") 
	(setf temp (read))
	
	(when (equalp 'y temp)
		(setf temp 'y)
	)
			
	(when (equalp 'n temp)
		(setf temp 'n)
	)
					
	(when (null *player*) 
		(print "Please enter 'y' or 'n'.")
		(endgame board)
	)
							
	)				
)


(defun test ()
;	(minimax *lst2* 2 *AIColor*)

	(setf butt (minimax *lst2* 2 *AIColor*) )
)

(othello *args*)



;know: (load 'othello)
;know: (test)
;know: (PrintOthello (nth 0 (nth 1 butt)))
