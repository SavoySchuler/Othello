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

	Program Description:



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

Function:	othello	

Description: 

	This function is the program main. It will call the the input function to
	have the user select a side. Once player colors are set, it will welcome 
	the user to the game and enter into the main game loop play-game(). When a
	termination case has been met, the function will move on to the score() and
	end-game() functions respectively to print the score and then either end the
	game or play it play it again. 

Usage:	(othello args)

	Where args is the command line parameter passed in to the input function to
	set the user's and AI's player colors.  
	
Returns: (end-game)

	Where end-game() is a function call to ask the player whether they want to
	end the game or play again. If the player wants to end the game, the program
	will quit. If they chose to play again it will call the othello() main 	
	function after resetting globals. If an invalid response is entered, it will
	call itself again. In this manner, no real return from this funciton exists.

Functions called:

	(score) - to calculate the final score of the game upon completion

	(end-game) - either terminates the game or start it again at the user's 
		selection

*****************************************************************************|#

(defun othello (args)
	(input args)
	(format t "~%Welcome to Othello. Let the match begin!~%")
	(play-game)	
	(score)
	(end-game)
)


#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:	play-game

Description: 
		
	This function is the primary loop of the main function othello. In general,
	it will perform the following four operations:

		1. Ask for, validate, and make move.
		2. Print board reflecting user's move.
		3. Make AI move.
		4. Print board reflecting AI's move. 
		5. Repeat loop. 

	If in the special case that the AI is the first player (black), the AI will 
	make its first move before entering this loop. There is a termination test
	at the start of each cycle to check if the game should end. Additionally,
	there are checks to see if the AI and the player have no move. If this case
	happens for both, it means the game has come to a halt and will terminate. 																	
Usage:	(play-game)	

Out:  (*board*)

	Where *board* is the global state of the game board modified every turn by
	a players move. 
	
Functions called:

	(print-othello *board*) - to print the board before every player move.

	(human-move userMove)	- valid and execute the user's move, else retry if 
		invalid move entered.

	(minimax *board* 2 *AIColor* -100000 100000 t)	- call to the AI's move,
		first calculated by minimax where alpha-beta pruning will begin at the
		initial state on the max level, and then applied to the game board,
		*board*.

*****************************************************************************|#

(defun play-game ()
	(let (userMove)		
		 
		; This special case handling will have the AI make the first move if it
		; is the first player before entering the main game loop.

			(when (equal *AIColor* 'b)
				(print-othello *board*)
				(format t "~%~%Opponent's move:")
				(setf lst (minimax *board* 2 *AIColor* -100000 100000 t))
				(when (not (null lst))
					(setf *board* (nth 0 (nth 1 lst)))
				)
				(format t "~%")		
			)		
		
		; Enter main game loop with enough turns for a full game
		(do ( ( i 0 (1+ i) ) )
			
			; Check if game should terminate. 
			(( >= i 30) ‘done)  ;termination test
			
			; Reset userMove each time for reading in user's move each turn.
			(setf userMove () )
			
			; Print board preceding user's move (will be AI's move each 
			; consequent turn)
			(format t "")	
			(print-othello *board*)		
			(format t "~%~%What is your move [row col]? ")
			
			; Read user move
			(setf userMove (append userMove (list (read))))
			(setf userMove (append userMove (list (read))))		
			
			; Make user's move
			(human-move userMove)

			; Call functions to make IA"s move	
			(format t "~%~%Opponent's move:")
			(setf lst (minimax *board* 2 *AIColor* -100000 100000 t))
			(when (not (null lst))
				(setf *board* (nth 0 (nth 1 lst)))
			)
			(format t "~%")		
	
			; Loop	
		)
	)
)



#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:	score	

Description: 
	
	This function is called upon completion of a game to count and display each 
	player's number of pieces left on the board. Depending on the user's color 
	choice and outcome, the function will display a 'win' or 'lose' message and 
	will always position the scores as "User - AI" for clarity. 	

Usage:	(score)

Out: (sumB sumW)

	Where sumB is the sum of the black player's pieces and sumW is the sum of
	the white player's pieces at  the end of the game. 	

Functions called: none

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

Function:	end-game

Description: 		

	This function is the last call of the othello main function and occurs at
	the completion (termination) of a game. It will prompt the user asking if 
	they would like to play again or quit. If the player chooses to play again,
	the global variables will be reset and (othello) will be called start the
	game again. If the player chooses to quit, the program will end. 

Usage:	(end-game)
	
Returns: none

	The function will either end the program if quit is selected or restart it
	if the player chooses to play again.

Functions called:

	(othello) - to restart the game, if selected

	(quit) - to end the program, selected

	(end-game) - a recursive call to itself until valid input is entered

*****************************************************************************|#

(defun end-game ()	
	(format t "Would you like to play again (y/n)?") 
	(setf playAgain (read))
	
	; When play again selected, reset the board position and call the program's
	; main function.
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
			
		; End program when player decides not to play again.
		(when (equalp 'n playAgain)
			(quit)
		)
	)

	; Recursively call end game until valid input is entered.
	end-game						
)				


;call to start game
(othello *args*)
