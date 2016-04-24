
#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:		 input

Description: 
	
	The input function is used upon program start up to register the player's
	chosen color (passed in as a command line argument) or prompt the player
	to select one. The player's color is stored in the global variable *player*.
	The AI's color is set opposite of the player's selection in the global 
	variable *AIColor*.


Parameters: args - command line argument (0 or 1)
			read - reads prompted user input (if 0 command line args)

Returns: 	*player* - sets player color (b or w)
			*AIColor* - sets AI color opposite of player color (b or W)


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
