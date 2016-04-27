#|********************************* othello.lsp *******************************

Program 3 - Othello Player vs. Artificial Intelligence Game

Authors: Savoy Schuler, Alex Nienheuser

Date: April 26, 2016

Professor: Dr. John Weiss

Course: CSC 447 - M001

Usage Instructions: clisp othello <optional player color: B, W, Black, or White>

Example Program Call:	clisp othello b

Bugs: 
	
Todo: 

Program Description:

	This program is a Lisp-based artificial	intelligence game designed to 
	compete in a game of othello against a human player. The player may select 
	whether they want to move first or second (a synonomous decision to choosing
	to be the black or white player, respectively) in either the command line
	arguments or when the game begins. The game will open with a welcoming 
	message to the player, then move to an input function to set player colors
	for the user and the AI. After giving each player their color, the game will
	move into a main loop, paly-game(), that will alternate turns between the 
	user and the AI, altering the board passed between each in a turn-to-turn 
	basis. When all positions are full or no move is left for either player the
	main game loop will terminate and the othello() function will move to a 
	score() function that calculates and displays the score, the winner, and the
	loser. After displaying this to the player, the main othello() function will
	move to the end-game() function to ask the player if they would to restart
	the program and play again or end it. 

	The AI program implements minimax in which it tries to pick the move that 
	maximizes its score while minimizing the move its opponent can pick on their
	next turn. This minimax is implemented with alpha-beta pruning which will
	limit the search tree heursitics performed to improve the efficiency of the
	program runtime. 

	The program implements three primary heuristics. The first of which is that
	the AI will value a position based upon its statistical worth in terms of
	board strategy. This is implemented by valuing the sum of board values 
	respective to one another when comparing possible board states. The second
	strategy is based off the observation that it is to the AI's advantage to
	not play for many pieces early in the game as the center pieces are volatile
	and will flip many times before the end of the game. If the center pieces
	are left to be mostly the opponent's, this will open the game up for many
	high scoring moves by the AI as it plays for high value edge and corner
	pieces. Lastly, the AI the will play for pieces that sit in between two
	opponent pieces as these offer optimum chances to flip the user's pieces
	as the game proceeds. 

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
(defvar *positionalStrat* '(999 -8 8 6 6 8 -8 999 -8 -24 -4 -3 -3 -4 -24 -8 8 -4 7 4 4 7 -4 8 6 -3 4 0 0 4 -3 6 6 -3 4 0 0 4 -3 6 8 -4 7 4 4 7 -4 8 -8 -24 -4 -3 -3 -4 -24 -8 999 -8 8 6 6 8 -8 999))
(defvar *board* '(
	- - - - - - - - 
	- - - - - - - - 
	- - - - - - - - 
	- - - W W - - - 
	- - - W W - - - 
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
	(let (userMove userMoveless aiMoveless)		
		 
		(setf userMoveless (player-no-move))
		(setf aiMoveless (AI-no-move))
	
		
		; Enter main game loop with enough turns for a full game
		(do ( ( i 0 (1+ i) ) )
			
			; Check if game should terminate. 
			( (and (eq userMoveless 0) (eq aiMoveless 0)))  ;termination test
			
			; Reset userMove each time for reading in user's move each turn.
			(setf userMove () )
			
			(when (equal userMoveless 1)
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
			)
			
			(setf userMoveless (player-no-move))
			(setf aiMoveless (AI-no-move))
			
			
			(when (equal userMoveless 1)
			; Call functions to make IA"s move
			(format t "~%~%Opponent's move:")
			(setf lst (minimax *board* 2 *AIColor* -100000 100000 t))
			(when (not (null lst))
				(setf *board* (nth 0 (nth 1 lst)))
			)
			(format t "~%")		
			)
			
			(setf userMoveless (player-no-move))
			(setf aiMoveless (AI-no-move))
	
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
	;Start counts at zero
	(let (sumB sumW)
	(setf sumW 0)
	(setf sumB 0)	

	; Count number of black and white pieces
	(dolist (tilePiece *board*)  
	 	(when (equal tilePiece 'B) 
			(setf sumB (+ sumB 1))
		)
	
		(when (equal tilePiece 'W)
			(setf sumW (+ sumW 1))
		)
	)

	;Print final board state
	(format t "~%~%~%Game over! Final board state: ~%")
	(print-othello *board*)

	; Print tie
	(when (eq sumB sumW)
		(format t "~%~%You tie! The score is ~a - ~a" sumB sumW)
	)

	;Print cases of win/lose
	(when (eq *player* 'b)
		(when (> sumB sumW)
			(format t "~%~%You win! The score is ~a - ~a" sumB sumW)
		)

		(when (< sumB sumW)
			(format t "~%~%You lose! The score is ~a - ~a" sumB sumW)
		)		
	)

	(when (eq *player* 'w)
		
		(when (< sumB sumW)
			(format t "~%~%You win! The score is ~a - ~a" sumW sumB)
		)
			
		(when (> sumB sumW)
			(format t "~%~%You lose! The score is ~a - ~a" sumW sumB)
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
	(format t "~%~%Would you like to play again (y/n)?") 
	(setf playAgain (read))
	
	; When play again selected, reset the board position and call the program's
	; main function.
		(when (equal 'y playAgain)
			(setf *board* '(
			- - - - - - - - 
			- - - - - - - - 
			- - - - - - - - 
			- - - W B - - - 
			- - - B W - - - 
			- - - - - - - - 
			- - - - - - - - 
			- - - - - - - -))	
			(setf *player* NIL)
			(setf *AIColor* NIL)
			(no-input)
			(othello *player*)
		)
			
		; End program when player decides not to play again.
		(when (equalp 'n playAgain)
			(quit)
		)
	; Recursively call end game until valid input is entered.
	end-game						
)				


#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:	make-move	

Description: 



Usage:	(othello args)

	
Returns: (end-game)


Functions called:



*****************************************************************************|#

(defun make-move (lst color ply)
		(let (newmove pos col row)
		(setf counter 0)	
		;Obtain list with new mov from AI
		(setf newmove (nth 0 (nth 1 (minimax lst ply color -100000 100000 t))))	
		(setf pos nil)
		
		;Loop through board until you find newmove
		(dotimes (indexX 63 0)
			(when (and (equal(nth indexX lst) '-) (not(equal(nth indexX newmove) '-)))
				;Once new move is found convert to row col
				(setf row (+ (floor(/ indexX 8)) 1))
				(setf col (+ (mod indexX 8) 1))
				(setf pos (list row col))
				
			)
		)
		pos
	)
)


;call to start game
(if (> (length *args*) 0)
	(othello *args*)
)
