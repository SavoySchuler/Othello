#|********************************* othello.lsp *******************************
	Program 3 - Othello Player vs. Artificial Intelligence Game

	Authors: Savoy Schuler, Alex Nienheuser

	Date: April 18, 2016

	Professor: Dr. John Weiss

	Course: CSC 447 - M001

	Usage Instructions: clisp othello <optional player color: B, W, Black, or White>

	Example Program Call:	clisp othello b

	Bugs: 
	
	Todo: 

	Program Details:



*****************************************************************************|#

(load (merge-pathnames "input.lsp" *load-truename*))
(load (merge-pathnames "othello-init.lsp" *load-truename*))
(load (merge-pathnames "minimax.lsp" *load-truename*))
(load (merge-pathnames "move-generator.lsp" *load-truename*))
(load (merge-pathnames "heuristics.lsp" *load-truename*))

(defvar *player* NIL)
(defvar *AIColor* NIL)
(defvar *edgeTopRow*      '(1 2 3 4 5 6 ))
(defvar *edgeLeftColumn* '(8 16 24 32 40 48 ))
(defvar *edgeBottomRow*   '(57 58 59 60 61 62 ))
(defvar *edgeRightColumn*  '(55 47 39 31 23 15))
(defvar *positionalStrat* '(99 -8 8 6 6 8 -8 99 -8 -24 -4 -3 -3 -4 -24 -8 8 -4 7 4 4 7 -4 8 6 -3 4 0 0 4 -3 6 6 -3 4 0 0 4 -3 6 8 -4 7 4 4 7 -4 8 -8 -24 -4 -3 -3 -4 -24 -8 99 -8 8 6 6 8 -8 99))
(defvar *board* '(
	- - - - - - - - 
	- - - - - - - - 
	- - - - - - - - 
	- - - W B - - - 
	- - - B W - - - 
	- - - - - - - - 
	- - - - - - - - 
	- - - - - - - -
) )



#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Parameters: 


Returns:


*****************************************************************************|#

(defun othello (args)
	(input args)
	(if
		(eq *player* 'b)
		(playerFirst)
		(opponentFirst)
	)	
	score
	endgame
)


#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Parameters: 


Returns:


*****************************************************************************|#

(defun playerFirst ()
	(let (userMove)		
		(do ( ( i 0 (1+ i) ) )
			(( >= i 10) ‘done)  							;termination test
			(PrintOthello *board*)		
			(format t "~%Please enter the coordinates of your move as (x y):")
			(setf userMove (read))
			(human-move userMove)	
			(move-generator *board* 'w)
		)	
	)
)


#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Parameters: 


Returns:


*****************************************************************************|#

(defun opponentFirst ()
	(let (userMove)	
		(do ( ( i 0 (1+ i) ) )
			(( >= i 10) ‘done)  							;termination test
			(PrintOthello *board*)			
			(move-generator *board* 'b)
			(format t "~%Please enter the coordinates of your move as (x y):")
			(setf userMove (read))
			(human-move userMove)	
		)
	)
)


#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Parameters: 


Returns:


*****************************************************************************|#

(defun score ()
	(let (playAgain enColor sumB sumW)
	(setf sum 0)	

	(dolist (tilePiece *board*)  
		
	 	(when (equal tilePiece 'b) 
			(setf sum (+ sumB 1))
		)
	
		(when (equal tilePiece 'w)
			(setf sum (+ sumW 1))
		)
	)

	(cond
		(when (eq sumB sumA)
			(format t "You tie! The score is ~a ~a" sumB sumA)
		)

		(when (eq *player* 'b)
			(cond			
				(when (> sumB sumW)
					(format t "You win! The score is ~a ~a" sumB sumA)
				)

				(when (< sumB sumW)
					(format t "You lose! The score is ~a ~a" sumB sumA)
				)
			)
		)

		(when (eq *player* 'w)
			(cond			
				(when (< sumB sumW)
					(format t "You win! The score is ~a ~a" sumA sumB)
				)

				(when (> sumB sumW)
					(format t "You lose! The score is ~a ~a" sumA sumB)
				)
			)
		)
	)
	)
)


#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Parameters: 


Returns:


*****************************************************************************|#

(defun endgame ()	
	(format t "Would you like to play again (y/n)?") 
	(setf playAgain (read))
	
	(cond 
		(when (equalp 'y playAgain)
			(setf *board* '(
			- - - - - - - - 
			- - - - - - - - 
			- - - - - - - - 
			- - - W B - - - 
			- - - B W - - - 
			- - - - - - - - 
			- - - - - - - - 
			- - - - - - - -))	
			(othello)
		)
			
		(when (equalp 'n playAgain)
			(quit)
		)
	)

	endgame						
)				



(defun test ()
;	(minimax *lst2* 2 *AIColor*)

	(setf butt (minimax *lst2* 2 *AIColor*) )
)

(othello *args*)


;know: (load 'othello)
;know: (test)
;know: (PrintOthello (nth 0 (nth 1 butt)))
