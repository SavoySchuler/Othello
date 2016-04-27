#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:	more-player-count	

Description: 

	This function is a state evaluation heuristic that returns a number 
	representing how many more player pieces are in a board state than opponent 
	pieces (naturally meaning a negative number will be returned if there are 
	more oppoent pieces than player pieces). The more positive a number 
	returned, the more advantageous the board state	is for the color passed in. 

Usage:	(more-player-count position color)

	Where position is a given board state and color is the color the player we 
	are seeking evaluation for. 

Returns: (sum)
	
	Where sum is the count of how many more player pieces than opponent pieces
	exist in a board state. 

Functions called: none

*****************************************************************************|#
	
(defun more-player-count (position color)
	(let (enColor sum)
		
		;Start sum at zero
		(setf sum 0)
		
		;Set values for player and enemy player
		(if (equal color 'w)
			(setf enColor 'b)
			(setf enColor 'w)
		)	

	; Iterate through the pieces on the board adding one for each player piece 
	; an subtracting one for each enemy piece
	(dolist (tilePiece position)  
		
	 	(when (equal color tilePiece) 
			(setf sum (+ sum 1))
		)
	
		(when (equal enColor tilePiece)
			(setf sum (- sum 1))
		)
	)

	; Return sum
	sum
	)	
)




#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function: position-strategy	

Description: 		

	This function is a state evaluation heuristic that sums the statistical
	value of board positions held (looking forward) for the player whose color
	is passed in and subtracts the corresponding statisitcal value of board
	positions held by the enemy player. The goal of the heuristic is to return
	a sum that represents the numerical value of a move choice to the static
	evaluation function. 

Usage:	(position-strategy position color)

	Where position is a given board state in question and color is the color of
	the player we are evaluating the state for. 

Returns: (sum)

	Where sum is value of the position being considered for selection 	

Functions called:

	(all-position) - finds all the positions of a specified color

*****************************************************************************|#

(defun position-strategy (position color)
	(let (lst sum enColor)
		
		(cond	
		((equal color 'w)
		(setq enColor 'b))

		((equal color 'w)
		(setq enColor 'b))		
	)
		
		(setf sum 0)
		(setf lst (all-positions color position))
		
		(dolist (indexX lst)
			(setf sum (+ sum (nth indexX *positionalStrat*)))
		)
		
		(setf lst (all-positions enColor position))
		
		(dolist (indexX lst)
			(setf sum (- sum (nth indexX *positionalStrat*)))
		)
		sum
	)
)



#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:	is-between	

Description: 		

		This function is a hueristic that will will add 30 if the AI is on the
		edge of the board and also between two enemy pieces.


Usage:		(is-between color position side offset) - This function takes a color
		that will will be checked if it's inbetween two pieces, a postion to check
		a side of the board to compare it too , and and offset.
	

Returns: 	-This will return a total based of the ability to move between to enemy	
		pieces while still on the edge.
	

Functions called:


*****************************************************************************|#

(defun is-between (color position side offset)
	(let (enColor total)
		(setf total 0)
		
		(if (equal color 'w)
			(setf enColor 'b)
			(setf enColor 'w)
		)
	
		(dolist (indexX side)
			(when (equal color (nth indexX position))
				(when (and (equal enColor (nth (- indexX offset) position))
						   (equal enColor (nth (+ indexX offset) position)))
					(setf total (+ total 30))
				)
			)
		)
		total
	)
)
