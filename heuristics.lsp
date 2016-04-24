#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Usage:		


Returns:	


Functions called:


*****************************************************************************|#

(defun more-player-count (position color)
	(let (enColor sum)
		(setf sum 0)
		
		(if (equal color 'w)
			(setf enColor 'b)
			(setf enColor 'w)
		)	


	(dolist (tilePiece position)  
		
	 	(when (equal color tilePiece) 
			(setf sum (+ sum 1))
		)
	
		(when (equal enColor tilePiece)
			(setf sum (- sum 1))
		)
	)
	sum
	)	
)


#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Usage:	
	

Returns:
	

Functions called:



*****************************************************************************|#

(defun more-opponent-count (position color)
	(let (enColor sum)
		(setf sum 0)
		
		(if (equal color 'w)
			(setf enColor 'b)
			(setf enColor 'w)
		)	

	(dolist (tilePiece position)  
		
	 	(when (equal color tilePiece) 
			(setf sum (- sum 1))
		)
	
		(when (equal enColor tilePiece)
			(setf sum (+ sum 1))
		)
	)
	sum
	)	
)


#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Usage:	
	

Returns:
	

Functions called:


*****************************************************************************|#

(defun position-strategy (position color)
	(let (lst sum)
		
		(setf sum 0)
		(setf lst (all-positions color position))
		
		(dolist (indexX lst)
			(setf sum (+ sum (nth indexX *positionalStrat*)))
		)
		
		sum
	)
)



#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Usage:	
	

Returns:
	

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
