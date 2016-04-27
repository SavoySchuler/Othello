#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:		 input

Description: 
	
	The input function is used upon program start up to register the player's
	chosen color (passed in as a command line argument) or pass handling to the
	no-input() function to prompt the user to select a player. The player's 
	color is stored in the global variable *player*. The AI's color is set 
	opposite of the player's selection in the global variable *AIColor*.

Usage:	(input args) 
	
	Where args is the command line argument passed in, if any, setting a player
	color for the user. If no command line argument is provided, handling will
	be passed to the no-inpute() function.
	
Out:	(*player* *AIColor*)

	Where *player* is the global variable that will be set to the user's
	chosen player color (b or w) and *AIColor* will be set to the opposite value
	for the	AI's player color. 
	
Functions called: 

	(no-input) - to ask the user to choose a color when no player color 
		specified by the command line argument

*****************************************************************************|#

(defun input(args)	
	(cond 
		((not (listp args))
		
		(when (equalp "Black" args)
				(setf *player* 'b)
			)
			
			(when (equalp "White" args)
				(setf *player* 'w)
			)

			(when (equalp 'b args)
				(setf *player* 'b)					
			)
			
			(when (equalp 'w args)
				(setf *player* 'w)
			)
					
			(when (null *player*)
				(no-input)
			)	
		)		
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
				(no-input)
			)						
		)
	
		((= (length args) 0)  
			(no-input)				
		)
	)

	(if (eq *player* 'w)
		(setf *AIColor* 'b)
		(setf *AIColor* 'w)
	)
)


#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:		no-input

Description: 
	
	When no player color is specified by the user at start-up, this function 
	will recursively prompt the user to select either black or white as a player
	color. Once a color has been selected, the function will set the global 
	*player* to the user's chosen color and will set *AIColor* opposite of the 
	player's chosen color.


Usage:	(no-input)
	
Out:	(*player* *AIColor*)

	Where *player* is the global variable that will be set to the user's
	chosen player color (b or w) and *AIColor* will be set to the opposite value
	for the	AI's player color. 
	
Functions called: 

	(no-input) - to recursively ask the user to choose a player color 

*****************************************************************************|#

(defun no-input ()
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
				(no-input)
			)
							
			)
	(if (eq *player* 'w)
		(setf *AIColor* 'b)
		(setf *AIColor* 'w)
	)
)	
