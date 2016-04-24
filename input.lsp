
#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:		 input

Description: 
	
	The input function is used upon program start up to register the player's
	chosen color (passed in as a command line argument) or prompt the player
	to select one. The player's color is stored in the global variable *player*.
	The AI's color is set opposite of the player's selection in the global 
	variable *AIColor*.


Usage:	(input args) 
	
	Where args is the command line argument passed in, if any, setting a player
	color for the user. If no command line argument is provided, the function
	will prompt the user for input, stored in read, which will be used to set
	the user's player color. 
	

Out:	(*player* *AIColor*)

	Where *player* is the global variable that will be set to the user's
	chosen player color (b or w) and *AIColor* will be set to the opposite value
	for the	AI's player color. 
	

Functions called: 

	(input args) - to recursively ask the user to select a player color if 
	invalid input is entered.

*****************************************************************************|#

(defun input(args)
	(cond 
		((= (length args) 1)  	

			(when (equalp "Black" (car args))
				(setf *player* 'b)
			)
			
			(when (equalp "White" (car args))
				(setf *player* 'w)
			)

			(when (equalp "b" (car args))
				(setf *player* 'b)					
			)
			
			(when (equalp "w" (car args))
				(setf *player* 'w)
			)
					
			(when (null *player*)
				(print "Usage: clisp Othello.lsp <player - 'Black' or 'White'>") 
			)
						

		)
	
		((= (length args) 0)  

			(format t "Would you like to move first [y/n]? ") 
			(let (temp)	(setf temp (read))
	
			(when (equalp 'y temp)
				(setf *player* 'b)
			)
			
			(when (equalp 'n temp)
				(setf *player* 'w)
			)
					
			(when (null *player*) 
				(print "Please enter 'y' or 'n'.")
				(input (car args))
			)
							
			)				
		)
	)

	(if (eq *player* 'w)
		(setf *AIColor* 'b)
		(setf *AIColor* 'w)
	)
)
