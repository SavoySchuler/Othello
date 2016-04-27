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

		; Catch for if entered numbers are too large
		(when (> (car userMove) 8)
		(format t "Row value too large, try again.")
			(play-game)
		)

		(when (> (cadr userMove) 8)
			(format t "Column value too large, try again.")
			(play-game)
		)

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

Function: print-othello		

Description: 		

	This function is used to print out a provided othello board state to the 
	command line. It will iterate though the provided list and output it to a 
	specified format. 
	
	Example:
		1 2 3 4 5 6 7 8
	1	- - - - - - - - 
	2	- - - - - - - - 
	3	- - - - - - - - 
	4	- - - W B - - - 
	5	- - - B W - - - 
	6	- - - - - - - - 
	7	- - - - - - - - 
	8	- - - - - - - -


Usage:	(print-othello *board*)
	

Returns: None
	

Functions called: (format t string variables)- A function that will output data



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

Function:	move-generator	

Description: 

		This function takes a othello board and a color, it will then generate 
		all possible moves for the given player on this board state. It will 
		look for all "-" around any enemy pieces. It will then look in all 
		straight lines verifing if it is a possible  using the check-all moves
		function.


Usage:	(move-generator lst color) - Pass a list consisting a board state and 
	color you would like to generate the moves for.
	

Returns:	A list of possible board positions as well. If there is not 
		possible board position it will return NIL.
	

Functions called (check-all-moves oth index color) - To check the position 
		of the given "-" and see if it can generate a valid move.
	




*****************************************************************************|#

(defun move-generator (oth color)
	(let (pos left right up down leftUp rightUp leftDown rightDown endColor children)
		
		;Get enemies color
		(if (equal color 'w)
			(setf endColor 'b)
			(setf endColor 'w)
		)
	
		(setf children nil)
	
		;find all enemy pieces positions
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
		
		;;Appened all moves
		(setf children (append children (list left right up down leftUp rightUp leftDown rightDown)))
		
		)
		
		;Remove all NIL's in list
		(setf children(remove nil children))
		
		;Remove duplicates
		(setf children(remove-duplicates children :test #'equal))
		
		
		children

	)
)


#|*****************************************************************************  
Authors: Alex Nienheuser, Savoy Schuler

Function:	all-positions

Description: 	

		This function will find all position of a color on an othello board. It
		will iterate through the board and check each piece to see if it is the 
		designated color and storing it in a list.


Usage:	(all-positions color lst) - Supply the color you want to find the 
		positions of and a board to search.


Returns: None
	

Functions called: None



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

Function:	check-all-moves	

Description: 	

		This functin will determine if a position on an othello board that is 
		currently "-" a valid move, as well as return generated move. It will 
		take an othello board, a position on the board, and color.


Usage:	(check-all-moves lst position color) - Pass a list consisting of an 
	othello board, a position, and the color of the move it should be.
	

Returns: This will return a board state with the given move applied.
		If there is no move to be made it will return NIL.
	

Functions called: (check-move-L oth indexX endColor) - Determind if a 
			move is valid moving left.
								
				(check-move-R oth indexX endColor) - Determind if a 
			move is valid moving right.

				(check-move-U oth indexX endColor) - Determind if a 
			move is valid moving up.
					
				(check-move-D oth indexX endColor) - Determind if a 
			move is valid moving down.
					
				(check-move-UL oth indexX endColor) - Determind if a 
			move is valid moving up and left.
					
				(check-move-UR oth indexX endColor) - Determind if a 
			move is valid moving up and right.
					
				(check-move-DL oth indexX endColor) - Determind if a 
			move is valid moving down and left.
					
				(check-move-DR oth indexX endColor) - Determind if a 
			move is valid moving down and right.
					
					
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

		;Create the board with the given moves
		(setf lst(create-move oth pos endPos endColor))
		
		lst
	)
)

#|*****************************************************************************  
Authors: Alex Nienheuser, Savoy Schuler

Function:	check-move-UL	

Description: 	

		This function looks through an othello board and moves up and left
		until it finds valid/invalid move. It does this by starting at a given
		position and moving through the list until it finds another piece of a 
		passed color 2 or more moves away. If it runs into an adjacent same 
		color piece, or a "-" it will then produce NIL and return it. If it 
		finds a valid move it will return the last position of that move.


Usage:		(check-move-UL lst position color) - Pass this function a list 
		containing a board state, a position on that board and a color. It will
		take this data determine if there is a move to be made up and left from 
		the given position.
	

Returns:	This function will return a the last to be changed of a given move 
		position,
	

Functions called: None


*****************************************************************************|#

(defun check-move-UL (oth pos color)
	(let (row)
	
		;Initailize the end position
		(setf endPos 7777777)
		
		(setf row (floor (/ pos 8)))
		
		(do ((indexX (- pos 9) (setf indexX (+ indexX '-9))))
			((or (not (equal endPos 7777777))(< indexX 0))
				)
			
			;Track rows
			(setf row (- row 1))
			
			;Did we find a valid move
			(when(and (not(equal indexX (- pos 9)))(equal (nth indexX oth) color))
				(setf endPos indexX)
			)
			
			;Did we determine an invalid move
			(when(or (equal (nth indexX oth) '-) (or (not(equal (floor (/ indexX 8)) row)) 
			(and (< indexX 0) (> indexX 63))))
				(setf endPos nil)
			)
			
			;Is the adjacent position of the passed color
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

Function:	check-move-UR	

Description: 	

		This function looks through an othello board and moves up and right
		until it finds valid/invalid move. It does this by starting at a given
		position and moving through the list until it finds another piece of a 
		passed color 2 or more moves away. If it runs into an adjacent same 
		color piece, or a "-" it will then produce NIL and return it. If it 
		finds a valid move it will return the last position of that move.


Usage:		(check-move-UL lst position color) - Pass this function a list 
		containing a board state, a position on that board and a color. It will
		take this data determine if there is a move to be made up and right from 
		the given position.
	

Returns:	This function will return a the last to be changed of a given move 
		position,
	

Functions called:None


*****************************************************************************|#

(defun check-move-UR (oth pos color)
	(let (row)
	
		(setf endPos 7777777)		
		
		(setf row (floor (/ pos 8)))
		
		(do ((indexX (- pos 7) (setf indexX (+ indexX '-7))))
			((or (not (equal endPos 7777777))(< indexX 0)))
			
			;Track the row
			(setf row (- row 1))
			
			;Did we find a valid moce
			(when(and (not(equal indexX (- pos 7)))(equal (nth indexX oth) color)) 
				(setf endPos indexX)
			)
			
			;Is the move determined to be invalid
			(when(or (equal (nth indexX oth) '-) (or (not(equal (floor (/ indexX 8)) row)) 
			(and (< indexX 0) (> indexX 63))))
				(setf endPos nil)
			)
			
			;Is the adjacent move the same as the passed color
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

Function:	check-move-DL	

Description: 	

		This function looks through an othello board and moves down and left
		until it finds valid/invalid move. It does this by starting at a given
		position and moving through the list until it finds another piece of a 
		passed color 2 or more moves away. If it runs into an adjacent same 
		color piece, or a "-" it will then produce NIL and return it. If it 
		finds a valid move it will return the last position of that move.


Usage:		(check-move-UL lst position color) - Pass this function a list 
		containing a board state, a position on that board and a color. It will
		take this data determine if there is a move to be made down and left from 
		the given position.
	

Returns:	This function will return a the last to be changed of a given move 
		position,
	

Functions called: None


*****************************************************************************|#

(defun check-move-DL (oth pos color)
	(let (row)
	
		(setf endPos 7777777)
		
		(setf row (floor (/ pos 8)))
		
		(do ((indexX (+ pos 7) (setf indexX (+ indexX '+7))))
			((or (not (equal endPos 7777777))(> indexX 63))
				)
			
			;Track row
			(setf row (+ row 1))
		
			;Is this the last move for a valid move
			(when(and (not(equal indexX (+ pos 7)))(equal (nth indexX oth) color)) 
				(setf endPos indexX)
			)
			
			;Is this an invalid move
			(when(or (equal (nth indexX oth) '-) (or (not(equal (floor (/ indexX 8)) row)) 
			(and (< indexX 0) (> indexX 63))))
				(setf endPos nil)
			)
			
			;Is the adjacent move the color
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

Function:	check-move-DR	

Description: 	

		This function looks through an othello board and moves down and right
		until it finds valid/invalid move. It does this by starting at a given
		position and moving through the list until it finds another piece of a 
		passed color 2 or more moves away. If it runs into an adjacent same 
		color piece, or a "-" it will then produce NIL and return it. If it 
		finds a valid move it will return the last position of that move.


Usage:		(check-move-UL lst position color) - Pass this function a list 
		containing a board state, a position on that board and a color. It will
		take this data determine if there is a move to be made down and right from 
		the given position.
	

Returns:	This function will return a the last to be changed of a given move 
		position,
	

Functions called: None


*****************************************************************************|#

(defun check-move-DR (oth pos color)
	(let (row)
	
		(setf endPos 7777777)
		
		(setf row (floor (/ pos 8)))
		
		(do ((indexX (+ pos 9) (setf indexX (+ indexX '+9))))
			((or (not (equal endPos 7777777))(> indexX 63))
				)
			
			;Track Row
			(setf row (+ row 1))
			
			;Is this the last move in a valid move
			(when(and (not(equal indexX (+ pos 9)))(equal (nth indexX oth) color)) 
				(setf endPos indexX)
			)
			
			;Is this an invalid move location
			(when(or (equal (nth indexX oth) '-) (or (not(equal (floor (/ indexX 8)) row)) 
			(and (< indexX 0) (> indexX 63))))
				(setf endPos nil)
			)
			
			;Is the adjacent piece the same color
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

Function:	check-move-D	

Description: 	

		This function looks through an othello board and moves down
		until it finds valid/invalid move. It does this by starting at a given
		position and moving through the list until it finds another piece of a 
		passed color 2 or more moves away. If it runs into an adjacent same 
		color piece, or a "-" it will then produce NIL and return it. If it 
		finds a valid move it will return the last position of that move.


Usage:		(check-move-UL lst position color) - Pass this function a list 
		containing a board state, a position on that board and a color. It will
		take this data determine if there is a move to be made down from 
		the given position.
	

Returns:	This function will return a the last to be changed of a given move 
		position,
	

Functions called: None


*****************************************************************************|#

(defun check-move-D (oth pos color)
	(let (row)
	
		(setf endPos 7777777)
		
		(setf row (floor (/ pos 8)))
		
		(do ((indexX (+ pos 8) (setf indexX (+ indexX '+8))))
			((or (not (equal endPos 7777777))(> indexX 63)))
			
			;Is this the last piece of a valid move
			(when(and (not(equal indexX (- pos 8)))(equal (nth indexX oth) color))
				(setf endPos indexX)
			)
			
			;Is this an invalid move
			(when(or (equal (nth indexX oth) '-) 
			(and (< indexX 0) (> indexX 63)))
				(setf endPos nil)
			)
			
			;Is the adjacent piece the same color
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

Function:	check-move-U	

Description: 	

		This function looks through an othello board and moves up
		until it finds valid/invalid move. It does this by starting at a given
		position and moving through the list until it finds another piece of a 
		passed color 2 or more moves away. If it runs into an adjacent same 
		color piece, or a "-" it will then produce NIL and return it. If it 
		finds a valid move it will return the last position of that move.


Usage:		(check-move-UL lst position color) - Pass this function a list 
		containing a board state, a position on that board and a color. It will
		take this data determine if there is a move to be made up from 
		the given position.
	

Returns:	This function will return a the last to be changed of a given move 
		position,
	

Functions called: None


*****************************************************************************|#

(defun check-move-U (oth pos color)
	(let (row)
	
		(setf endPos 7777777)
		
		(setf row (floor (/ pos 8)))
		
		(do ((indexX (- pos 8) (setf indexX (+ indexX '-8))))
			((or (not (equal endPos 7777777)) (< indexX 0))
				)
			
			;Is this the last piece in a valid move
			(when(and (equal indexX (- pos 8))(equal (nth indexX oth) color))
				(setf endPos nil)
			)
			
			;Is this an invalid move
			(when(and (not(equal indexX (- pos 8)))(equal (nth indexX oth) color))
				(setf endPos indexX)
			)
			
			;Is the adjacent piece the same color
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

Function:	check-move-R	

Description: 	

		This function looks through an othello board and moves right
		until it finds valid/invalid move. It does this by starting at a given
		position and moving through the list until it finds another piece of a 
		passed color 2 or more moves away. If it runs into an adjacent same 
		color piece, or a "-" it will then produce NIL and return it. If it 
		finds a valid move it will return the last position of that move.


Usage:		(check-move-UL lst position color) - Pass this function a list 
		containing a board state, a position on that board and a color. It will
		take this data determine if there is a move to be made right from 
		the given position.
	

Returns:	This function will return a the last to be changed of a given move 
		position,
	

Functions called: None


*****************************************************************************|#

(defun check-move-R (oth pos color)
	(let (row)
	
		(setf endPos 7777777)
		
		(setf row (floor (/ pos 8)))
		
		(do ((indexX (+ pos 1) (setf indexX (+ indexX 1))))
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

Function:	check-move-L	

Description: 	

		This function looks through an othello board and moves left
		until it finds valid/invalid move. It does this by starting at a given
		position and moving through the list until it finds another piece of a 
		passed color 2 or more moves away. If it runs into an adjacent same 
		color piece, or a "-" it will then produce NIL and return it. If it 
		finds a valid move it will return the last position of that move.


Usage:		(check-move-UL lst position color) - Pass this function a list 
		containing a board state, a position on that board and a color. It will
		take this data determine if there is a move to be made left from 
		the given position.
	

Returns:	This function will return a the last to be changed of a given move 
		position,
	

Functions called: None


*****************************************************************************|#

(defun check-move-L (oth pos color)
	(let (row)
	
		(setf endPos 7777777)
		
		(setf row (floor (/ pos 8)))
		
		(do ((indexX (- pos 1) (setf indexX (- indexX 1))))
			((or (not (equal endPos 7777777)) (< indexX 0))
				)
			
			;Is this the last piece in a valid move
			(when(and (not(equal indexX (- pos 1)))(equal (nth indexX oth) color)) 
				(setf endPos indexX)
			)
			
			;Is this an invalid move
			(when(or (equal (nth indexX oth) '-) (or (not(equal (floor (/ indexX 8)) row)) 
			(and (< indexX 0) (> indexX 63))))
				(setf endPos nil)
			)
			
			;Is the adjacent piece the same color
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

Function:		create-move

Description: 	

		This function will modify a board state and produce a valid move. It 
		will take board and apply a move based of an initial position and a list
		of end positions. These end position will indicate how far an index will
		move. With each move of the given indexX it will change the color of the 
		pieces on the board until it arrives at its given end position. It will
		do this for each position in the endPos list.


Usage:	(create-move lst pos endPos endColor)
	

Returns: 	This function will return a board state if atleast one valid move can
		be made, if not then it will return NIL.
	

Functions called: None


*****************************************************************************|#

(defun create-move (lst pos endPos endColor)
	(let (tempList offsets valid)
		(setf valid nil)

		(setf tempList (copy-list lst))
		
		(setf offsets '(1 -1 8 -8 9 7 -7 -9))
		
		(do
			((indexY 0 (setf indexY (+ indexY 1))))
			((>= indexY 8))
			
			;If there is an end position to move begin looping through it.
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
		
		;Set the initial position to the desired color.
		(setf (nth pos tempList) endColor)
		
		;if there is no valid move made return nil
		(when (null valid)
			(setf tempList nil)
		)
		
		
		tempList
	)
)


#|*****************************************************************************  

Authors: Alex Nienheuser, Savoy Schuler

Function:		AI-no-move

Description: 	

		This function will generate all successors for the AI Player and 
		determine if it has any moves to make. If so it returns 1, if not it 
		returns NIL.


Usage:		(AI-no-move) - This function takes no arguements
	

Returns:	This function will determine if AI Player can move. If so it
		returns 1, if not it returns NIL.
	

Functions called:	(move-generator *board* *AIColor*) - This will determine if
			the AI Player has any possible moves to make.


*****************************************************************************|#

(defun AI-no-move ()
	(if (null (move-generator *board* *AIColor*))
		0
		1
	)

)

#|*****************************************************************************  

Authors: Alex Nienheuser, Savoy Schuler

Function:		player-no-move

Description: 	

		This function will generate all successors for the Player and 
		determine if it has any moves to make. If so it returns 1, if not it 
		returns NIL.


Usage:		(AI-no-move) - This function takes no arguements
	

Returns:	This function will determine if Player can move. If so it
		returns 1, if not it returns NIL.
	

Functions called:	(move-generator *board* *player*) - This will determine if
			the player has any possible moves to make.


*****************************************************************************|#

(defun player-no-move ()
	(if (null (move-generator *board* *player*))
		0
		1
	)

)

