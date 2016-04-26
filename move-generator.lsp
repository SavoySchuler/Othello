#|*****************************************************************************  
Authors: Alex Nienheuser, Savoy Schuler

Function:	human-move

Description: 		

	This function is used to make the user's specified move. It will read in 
	the row column value of the user's specified move and convert it to a board
	index. It will then check to see if the move is legal. If the move is not
	legal, it will print a warning and return to play-game() to fetch a new move
	entered by the user. If the move is legal, it will alter the board state and
	print the current board to the screen to reflect this change to the user.
	The end of the function will singal a return to the main game loop 
	play-game() to continue playing.

Usage:	(human-move userMove)
	
	Where userMove is a list holding the row column value of the move the user
	would like to make. 

Out: (print-othello *board*)
	
	Where the board display shows the result of the user playing their move.

Functions called:

	(check-all-moves *board* pos *player*) - to check the legality of the user's
		chosen move.

	(play-game) - recursively get userMove if the entered move is invalid

	(print-othello *board*) - to print the board after the user move has been
		placed.

*****************************************************************************|#

(defun human-move (userMove)
	(let (pos posCol posRow lst)

		; Convert user input to board position - this step could be done in one
		; line, but is broken apart for readability.
		(setf posRow (* (- (car userMove) 1) 8))
		(setf posCol (- (cadr userMove) 1))
		(setf pos (+ posCol posRow))
	
		;check for legality of the user's move
		(setf lst (check-all-moves *board* pos *player*))
		(cond 		
			((null lst)
				; If invalid, return to play-game to call again
				(format t "Invalid move, try again.")		
		 		(play-game)
			)		
			
			; Else, if valid, make the the move and return the board displaying 
			; it.
			(t 
				(setf *board* lst)
				(format t "~%Your move:")
				(print-othello *board*)
			)
		)		
	)
)


#|*****************************************************************************  
Authors: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Usage:	
	

Returns:
	

Functions called:



*****************************************************************************|#
	
(defun print-othello (oth)
;	(setf *lst2* oth)
	(format t "~%   1 2 3 4 5 6 7 8")
	;Loop through orthello lst
	(dotimes (indexX (* 8 8) 0)
	
		(when (eq (mod indexX 8) 0)
			(format t "~% ~D " (+ 1 (floor (/ indexX 8))))
		)
	
		(format t "~s "(nth indexX oth))
	
	)
)


#|*****************************************************************************  
Authors: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Usage:	
	

Returns:
	

Functions called:


*****************************************************************************|#

;Not finished functions swp
;color - other players color
(defun move-generator (oth color)
	(let (pos left right up down leftUp rightUp leftDown rightDown endColor children)
		(if (equal color 'w)
			(setf endColor 'b)
			(setf endColor 'w)
		)
	
		(setf children nil)
	
		(setf pos (all-positions endColor oth))
	
		(dolist (indexX pos)
		
		;Determine if the left sucessor can be generated
		(when (and (> (mod indexX 8) 0)
			 (not (eq indexX 0) ))
			
			(when (eq (nth (- indexX 1) oth)'-)
				(check-all-moves oth (- indexX 1) color)
			)
			
		)

		
		;Determine if the right sucessor can be generated
		(when (< (mod indexX 8) (- 8 1))	
			(when (eq (nth (+ indexX 1) oth)'-)
					(setf right(check-all-moves oth (+ indexX 1) color))
			)	
		)

		
		;Determine if the up sucessor can be generated 
		(when (>= (/ indexX (float 8)) 1)
		
			(when (eq (nth (- indexX 8) oth)'-)
					(setf up (check-all-moves oth (- indexX 8) color))
			)
				
		)

		
		;Determine if the down sucessor can be generated
		(when (< (/ indexX (float 8)) (- 8 1))
		
			(when (eq (nth (+ indexX 8) oth)'-)
					(setf down(check-all-moves oth (+ indexX 8) color))
			)
		)
		
		
		
		;Determine if the up left sucessor can be generated
		(when (and (and (> (mod indexX 8) 0)
			(not (eq indexX 0) )) (>= (/ indexX (float 8)) 1))
			 
			(when (eq (nth (- indexX 9) oth)'-)
					(setf leftUp(check-all-moves oth (- indexX 9) color))
			)
		)
		
		
		;Determine if the up right sucessor can be generated
		(when (and (< (mod indexX 8) (- 8 1)) (>= (/ indexX (float 8)) 1))
			
			(when (eq (nth (- indexX 7) oth)'-)
					(setf rightUp(check-all-moves oth (- indexX 7) color))
			)

		)
		
		
		;Determine if the down left sucessor can be generated
		(when (and (and (> (mod indexX 8) 0)
			(not (eq indexX 0) )) (< (/ indexX (float 8)) (- 8 1)))
			
			(when (eq (nth (+ indexX 7) oth)'-)
				(setf leftDown(check-all-moves oth (+ indexX 7) color))
			)

		)
		
		
		;Determine if the down right sucessor can be generated
		(when (and (< (mod indexX 8) (+ 8 1)) (< (/ indexX (float 8)) (- 8 1)))
			
			(when (eq (nth (+ indexX 9) oth)'-)
					(setf rightDown(check-all-moves oth (+ indexX 9) color))
			)
		)
		
		
		(setf children (append children (list left right up down leftUp rightUp leftDown rightDown)))
		
		)
		

		(setf children(remove nil children))
		(setf children(remove-duplicates children :test #'equal))
		
		
		children

	)
)


#|*****************************************************************************  
Authors: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Usage:	
	

Returns:
	

Functions called:



*****************************************************************************|#

(defun all-positions (color oth)
  (loop
    for element in oth 
    and position from 0
     when (eql element color)
	  collect position)

)


#|*****************************************************************************  
Authors: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Usage:	
	

Returns:
	

Functions called:



*****************************************************************************|#

(defun check-all-moves (oth pos endColor)
(let (indexX lst left right up down leftUp rightUp leftDown rightDown)

		(setf indexX pos)	
		
		
		;Determine if the left sucessor can be generated
		(when (and (> (mod indexX 8) 0)
			 (not (<= indexX 0) ))
			 
			(setf left (check-move-L oth indexX endColor))
		)
		
		;Determine if the right sucessor can be generated
		(when (< (mod indexX 8) (- 8 1))
			(setf right (check-move-R oth indexX endColor ))
		)

		;Determine if the up sucessor can be generated 
		(when (>= (/ indexX (float 8)) 1)	
			(setf up (check-move-U oth indexX endColor ))
		)
		
		;Determine if the down sucessor can be generated
		(when (< (/ indexX (float 8)) (- 8 1))
			(setf down (check-move-D oth indexX endColor))
		)
		
		;Determine if the up left sucessor can be generated
		(when (and (and (> (mod indexX 8) 0)
			(not (<= indexX 0) )) (>= (/ indexX (float 8)) 1))			 
			(setf leftUp (check-move-UL oth indexX endColor))
		)
		
		;Determine if the up right sucessor can be generated
		(when (and (< (mod indexX 8) (- 8 1)) (>= (/ indexX (float 8)) 1))			
			(setf rightUp (check-move-UR oth indexX endColor))
		)

		;Determine if the down left sucessor can be generated
		(when (and (and (> (mod indexX 8) 0)
			 (not (<= indexX 0) )) (< (/ indexX (float 8)) (- 8 1)))	 
			(setf leftDown (check-move-DL oth indexX endColor))
		)

		;Determine if the down right sucessor can be generated
		(when (and (< (mod indexX 8) (- 8 1)) (< (/ indexX (float 8)) (- 8 1)))
			
			(setf rightDown(check-move-DR oth indexX endColor))
	
		)

		(setf endPos (list left right up down leftUp rightUp leftDown rightDown))
		(when (and (< indexX 64)(> indexX -1)) 
			(when (not (eq (nth indexX oth) '-))
				(setf endPos nil)
			)
		)

		(setf lst(create-move oth pos endPos endColor))
		
		lst
	)
)

#|*****************************************************************************  
Authors: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Usage:	
	

Returns:
	

Functions called:


*****************************************************************************|#

(defun check-move-UL (oth pos color)
	(let (row)
	
		(setf endPos 7777777)
		
		(setf row (floor (/ pos 8)))
		
		(do ((indexX (- pos 9) (setf indexX (+ indexX '-9))))
					 ; did we find a position/       make sure we dont step off the edge
			((or (not (equal endPos 7777777))(< indexX 0))
				)
			
			(setf row (- row 1))
			
			(when(and (not(equal indexX (- pos 9)))(equal (nth indexX oth) color))
				(setf endPos indexX)
			)
			
			(when(or (equal (nth indexX oth) '-) (or (not(equal (floor (/ indexX 8)) row)) 
			(and (< indexX 0) (> indexX 63))))
				(setf endPos nil)
			)
			
			(when(and (equal indexX (- pos 9))(equal (nth indexX oth) color))
				(setf endPos nil)
			)
			
		)
		(if (eq endPos 7777777)
			nil
			endPos
		)		
	)
)

#|*****************************************************************************  
Authors: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Usage:	
	

Returns:
	

Functions called:



*****************************************************************************|#

(defun check-move-UR (oth pos color)
	(let (row)
	
		(setf endPos 7777777)		
		
		(setf row (floor (/ pos 8)))
		
		(do ((indexX (- pos 7) (setf indexX (+ indexX '-7))))
					 ; did we find a position/       make sure we dont step off the edge
			((or (not (equal endPos 7777777))(< indexX 0)))
			
			(setf row (- row 1))
			
			(when(and (not(equal indexX (- pos 7)))(equal (nth indexX oth) color)) 
				(setf endPos indexX)
			)
			
			(when(or (equal (nth indexX oth) '-) (or (not(equal (floor (/ indexX 8)) row)) 
			(and (< indexX 0) (> indexX 63))))
				(setf endPos nil)
			)
			
			(when(and (equal indexX (- pos 7))(equal (nth indexX oth) color))
				(setf endPos nil)
			)
		)
		(if (eq endPos 7777777)
			nil
			endPos
		)				
	)
)

#|*****************************************************************************  
Authors: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Usage:	
	

Returns:
	

Functions called:



*****************************************************************************|#

(defun check-move-DL (oth pos color)
	(let (row)
	
		(setf endPos 7777777)
		
		(setf row (floor (/ pos 8)))
		
		(do ((indexX (+ pos 7) (setf indexX (+ indexX '+7))))
					 ; did we find a position/       make sure we dont step off the edge
			((or (not (equal endPos 7777777))(> indexX 63))
				)
			
			(setf row (+ row 1))
		
			(when(and (not(equal indexX (+ pos 7)))(equal (nth indexX oth) color)) 
				(setf endPos indexX)
			)
			
			(when(or (equal (nth indexX oth) '-) (or (not(equal (floor (/ indexX 8)) row)) 
			(and (< indexX 0) (> indexX 63))))
				(setf endPos nil)
			)
			
			(when(and (equal indexX (+ pos 7))(equal (nth indexX oth) color))
				(setf endPos nil)
			)
			
		)
		(if (eq endPos 7777777)
			nil
			endPos
		)			
	)
)


#|*****************************************************************************  
Authors: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Usage:	
	

Returns:
	

Functions called:


*****************************************************************************|#

(defun check-move-DR (oth pos color)
	(let (row)
	
		(setf endPos 7777777)
		
		(setf row (floor (/ pos 8)))
		
		(do ((indexX (+ pos 9) (setf indexX (+ indexX '+9))))
					 ; did we find a position/       make sure we dont step off the edge
			((or (not (equal endPos 7777777))(> indexX 63))
				)
			
			(setf row (+ row 1))
			
			(when(and (not(equal indexX (+ pos 9)))(equal (nth indexX oth) color)) 
				(setf endPos indexX)
			)
			
			(when(or (equal (nth indexX oth) '-) (or (not(equal (floor (/ indexX 8)) row)) 
			(and (< indexX 0) (> indexX 63))))
				(setf endPos nil)
			)
			
			(when(and (equal indexX (+ pos 9))(equal (nth indexX oth) color))
				(setf endPos nil)
			)
		
		)
		(if (eq endPos 7777777)
			nil
			endPos
		)			
	)
)


#|*****************************************************************************  
Authors: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Usage:	
	

Returns:
	

Functions called:



*****************************************************************************|#

(defun check-move-D (oth pos color)
	(let (row)
	
		(setf endPos 7777777)
		
		(setf row (floor (/ pos 8)))
		
		(do ((indexX (+ pos 8) (setf indexX (+ indexX '+8))))
					 ; did we find a position
			((or (not (equal endPos 7777777))(> indexX 63)))
			
			(when(and (not(equal indexX (- pos 8)))(equal (nth indexX oth) color))
				(setf endPos indexX)
			)
			
			(when(or (equal (nth indexX oth) '-) 
			(and (< indexX 0) (> indexX 63)))
				(setf endPos nil)
			)
			
			(when(and (equal indexX (+ pos 8))(equal (nth indexX oth) color))
				(setf endPos nil)
			)
			
		)
		
		(if (eq endPos 7777777)
			nil
			endPos
		)	
	)
)


#|*****************************************************************************  
Authors: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Usage:	
	

Returns:
	

Functions called:



*****************************************************************************|#

(defun check-move-U (oth pos color)
	(let (row)
	
		(setf endPos 7777777)
		
		(setf row (floor (/ pos 8)))
		
		(do ((indexX (- pos 8) (setf indexX (+ indexX '-8))))
					 ; did we find a position
			((or (not (equal endPos 7777777)) (< indexX 0))
				)
			
			(when(and (equal indexX (- pos 8))(equal (nth indexX oth) color))
				(setf endPos nil)
			)
			
			(when(and (not(equal indexX (- pos 8)))(equal (nth indexX oth) color))
				(setf endPos indexX)
			)
			
			(when(or (equal (nth indexX oth) '-) 
			(and (<= indexX 0) (> indexX 63)))
				(setf endPos nil)
			)
			
			
		)
				
	)
		
		(if (eq endPos 7777777)
			nil
			endPos
		)
	;endPos
)


#|*****************************************************************************  
Authors: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Usage:	
	

Returns:
	

Functions called:



*****************************************************************************|#

(defun check-move-R (oth pos color)
	(let (row)
	
		(setf endPos 7777777)
		
		(setf row (floor (/ pos 8)))
		
		(do ((indexX (+ pos 1) (setf indexX (+ indexX 1))))
					 ; did we find a position
			((or (not (equal endPos 7777777))(>= indexX 64))
				)
			
			(when(and (not(equal indexX (+ pos 1)))(equal (nth indexX oth) color))
				(setf endPos indexX)
			)
			
			(when(or (equal (nth indexX oth) '-) (or (not(equal (floor (/ indexX 8)) row)) 
			(and (< indexX 0) (> indexX 63))))
				(setf endPos nil)
			)
			
			(when(and (equal indexX (+ pos 1))(equal (nth indexX oth) color))
				(setf endPos nil)
			)

		)

		(if (eq endPos 7777777)
			nil
			endPos
		)	
	)
)


#|*****************************************************************************  
Authors: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Usage:	
	

Returns:
	

Functions called:



*****************************************************************************|#

(defun check-move-L (oth pos color)
	(let (row)
	
		(setf endPos 7777777)
		
		(setf row (floor (/ pos 8)))
		
		(do ((indexX (- pos 1) (setf indexX (- indexX 1))))
					 ; did we find a positionge
			((or (not (equal endPos 7777777)) (< indexX 0))
				)
			
			(when(and (not(equal indexX (- pos 1)))(equal (nth indexX oth) color)) 
				(setf endPos indexX)
			)
			
			(when(or (equal (nth indexX oth) '-) (or (not(equal (floor (/ indexX 8)) row)) 
			(and (< indexX 0) (> indexX 63))))
				(setf endPos nil)
			)
			
			(when(and (equal indexX (- pos 1))(equal (nth indexX oth) color))
				(setf endPos nil)
			)
		)
		
		(if (eq endPos 7777777)
			nil
			endPos
		)
		
	)

)


#|*****************************************************************************  

Authors: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Usage:	
	

Returns:
	

Functions called:


*****************************************************************************|#

(defun create-move (lst pos endPos endColor)
	(let (tempList offsets valid)
		(setf valid nil)
;		(setf endColor 'b)
		(setf tempList (copy-list lst))
		
		(setf offsets '(1 -1 8 -8 9 7 -7 -9))
		
		(do
			((indexY 0 (setf indexY (+ indexY 1))))
			((>= indexY 8))
			
			(when (not (null (nth indexY endPos)))
				(do 
				((indexX (nth indexY endPos) (setf indexX (+ indexX (nth indexY offsets)))))
				((equal (nth indexX tempList) '-))
				(setf valid 0)
				(setf (nth (nth indexY endPos) tempList) endColor)
				(setf (nth indexX tempList) endColor)
				)
			)
		)
		
		(setf (nth pos tempList) endColor)
		
		(when (null valid)
			(setf tempList nil)
		)
		
		
		tempList
	)
)


#|*****************************************************************************  

Authors: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Usage:	
	

Returns:
	

Functions called:


*****************************************************************************|#

(defun AI-no-move ()
	(if (null (move-generator *board* *player*))
		0
		1
	)

)

#|*****************************************************************************  

Authors: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Usage:	
	

Returns:
	

Functions called:


*****************************************************************************|#

(defun player-no-move ()
	(if (null (move-generator *board* *AIColor*))
		0
		1
	)

)



