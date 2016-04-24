#|**************************************************************************
 Filename: orthello.lsp


 Author: Alex Nienhueser, Savoy  Schuler


 Description:


*****************************************************************************|#

;un comment ctrl+k+u
;Todo Special case for rows and columns for check move
;Program thinks that two pieces next to one another is a valid move



;(defvar *lst2* '(- - - - - - - - - - - - - - - - - - - - - - - - - - - W B - - - - - - B W - - - - - - - - - - - - - - - - - - - - - - - - - - -))
 (defvar *lst2* '(- - - - - - - - - - - - - - - - - - - - - - - - - - B W B - - - - - - B W - - - - - - - - - - - - - - - - - - - - - - - - - - -))
;UpRight and UpLeft Test 
 ;  (setf *lst2* '(- - - - - - - - - - - - - - - - - - B - - W - - - - - W B - - - - - - B W - - - - - - - - - - - - - - - - - - - - - - - - - - -))
 ;DownRight and DownLeft Test 
;    (setf *lst2* '(- - - - - - - - - - - - - - - - - - - - - - - - - - - W B - - - - - - B W - - - - - W - - B - - - - - - - - - - - - - - - - - -))
;Test Down and Right
;   (setf *lst2* '(- - - B - - - - - - - W - - - - B W W - W B - - - - - W B - - - - - - B W - - - - - W - - B - - - - - - - - - - - - - - - - - -))
	
;ALL test
    (setf *lst2* '(- B - B - B - - - - W W W - - - B W W - W B - - - - W W W - - - - B - B W B B - - - W - - B - - - - - - - - - - - - - - - - - -)) 
;	(setf *lst2* '(- B W B W B - - - - W W W - - - B W W - W B - - - - W W W - - - - B - B W B B - - - W - - B - W - - - - - - - - - - - - - W B W))


#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Parameters: 


Returns:


*****************************************************************************|#

(defun human-move (userMove)
	(let (pos posCol posRow lst)
		;convert user input to board position	
		;This step could be done in one line, but is broken apart for readability
		(setf posRow (* (- (car userMove) 1) 8))
		(setf posCol (- (cadr userMove) 1))
		(setf pos (+ posCol posRow))
		(print pos)
	
		;check for legality
		
		;place move in board 
		;(setf (nth pos *board*) *player*)
		
		(setf lst (CheckAllMoves *board* pos *player*))
		(print "Success")
		(cond 		
		((null lst)
			(format t "Invalid move, try again.")		
		 	(player-first)
		)		
		(t (setf *board* lst))
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
	
(defun PrintOthello (oth)
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
Author: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Parameters: 


Returns:


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
	
		(setf pos (AllPositions endColor oth))
	
		(dolist (indexX pos)
		
		;Determine if the left sucessor can be generated
		(when (and (> (mod indexX 8) 0)
			 (not (eq indexX 0) ))
			
			(when (eq (nth (- indexX 1) oth)'-)
				(CheckAllMoves oth (- indexX 1) color)
			)
			
		)

		
		;Determine if the right sucessor can be generated
		(when (< (mod indexX 8) (- 8 1))
		
		(when (<= indexX 0)
			(print "It's going to start working?")
			(print indexX)
		)
		
			(when (eq (nth (+ indexX 1) oth)'-)
					(setf right(CheckAllMoves oth (+ indexX 1) color))
			)
			(when (<= indexX 0)
			(print "It worked?")
			(print indexX)
		)
		
		)

		
		;Determine if the up sucessor can be generated 
		(when (>= (/ indexX (float 8)) 1)
		
			(when (eq (nth (- indexX 8) oth)'-)
					(setf up (CheckAllMoves oth (- indexX 8) color))
			)
				
		)

		
		;Determine if the down sucessor can be generated
		(when (< (/ indexX (float 8)) (- 8 1))
		
			(when (eq (nth (+ indexX 8) oth)'-)
					(setf down(CheckAllMoves oth (+ indexX 8) color))
			)
		)
		
		
		
		;Determine if the up left sucessor can be generated
		(when (and (and (> (mod indexX 8) 0)
			(not (eq indexX 0) )) (>= (/ indexX (float 8)) 1))
			 
			(when (eq (nth (- indexX 9) oth)'-)
					(setf leftUp(CheckAllMoves oth (- indexX 9) color))
			)
		)
		
		
		;Determine if the up right sucessor can be generated
		(when (and (< (mod indexX 8) (- 8 1)) (>= (/ indexX (float 8)) 1))
			
			(when (eq (nth (- indexX 7) oth)'-)
					(setf rightUp(CheckAllMoves oth (- indexX 7) color))
			)

		)
		
		
		;Determine if the down left sucessor can be generated
		(when (and (and (> (mod indexX 8) 0)
			(not (eq indexX 0) )) (< (/ indexX (float 8)) (- 8 1)))
			
			(when (eq (nth (+ indexX 7) oth)'-)
				(setf leftDown(CheckAllMoves oth (+ indexX 7) color))
			)

		)
		
		
		;Determine if the down right sucessor can be generated
		(when (and (< (mod indexX 8) (+ 8 1)) (< (/ indexX (float 8)) (- 8 1)))
			
			(when (eq (nth (+ indexX 9) oth)'-)
					(setf rightDown(CheckAllMoves oth (+ indexX 9) color))
			)
		)
		
		
		(setf children (append children (list left right up down leftUp rightUp leftDown rightDown)))
		
		)
		
;		(PrintOthello left) 
;		(PrintOthello right) 
;		(PrintOthello up) 
;		(PrintOthello down) 
;		(PrintOthello leftUp)
;		(PrintOthello rightUp)
;		(PrintOthello leftDown)
;		(PrintOthello rightDown)
		(setf children(remove nil children))
		(setf children(remove-duplicates children :test #'equal))
		
;		(print "")
;		(print endColor)
;		(print "Turn")
;		(print "Printing Othello")
;		(PrintOthello oth)
;		(dolist (indexX children)
;			(PrintOthello indexX)
;		)
		
		children

	)
)


#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Parameters: 


Returns:


*****************************************************************************|#

(defun AllPositions (color oth)
  (loop
    for element in oth 
    and position from 0
     when (eql element color)
	  collect position)

)


#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Parameters: 


Returns:


*****************************************************************************|#

(defun CheckAllMoves (oth pos endColor)
(let (indexX lst left right up down leftUp rightUp leftDown rightDown)
;		(if (eq color 'w)
;			(setf endColor 'b)
;			(setf endColor 'w)
;		)
	
		;(setf pos (AllPositions color oth))
		(setf indexX pos)
		
		
		(when (<= indexX 0)
			(print "At zero")
			(print indexX)
		)
		
		
		;Determine if the left sucessor can be generated
		(when (and (> (mod indexX 8) 0)
			 (not (<= indexX 0) ))
			 
			 (print "starting poopy")
			 (print indexX)
			(setf left (CheckMoveL oth indexX endColor))
			(print "Poopy buttz")
		)
		
		;Determine if the right sucessor can be generated
		(when (< (mod indexX 8) (- 8 1))
			(setf right (CheckMoveR oth indexX endColor ))
		)

		;Determine if the up sucessor can be generated 
		(when (>= (/ indexX (float 8)) 1)		
			(setf up (CheckMoveU oth indexX endColor ))
		)
		
		;Determine if the down sucessor can be generated
		(when (< (/ indexX (float 8)) (- 8 1))
			(setf down (CheckMoveD oth indexX endColor))
		)
		
		;Determine if the up left sucessor can be generated
		(when (and (and (> (mod indexX 8) 0)
			(not (<= indexX 0) )) (>= (/ indexX (float 8)) 1))			 
			(setf leftUp (CheckMoveUL oth indexX endColor))
		)
		
		;Determine if the up right sucessor can be generated
		(when (and (< (mod indexX 8) (- 8 1)) (>= (/ indexX (float 8)) 1))			
			(setf rightUp (CheckMoveUR oth indexX endColor))
		)

		;Determine if the down left sucessor can be generated
		(when (and (and (> (mod indexX 8) 0)
			 (not (<= indexX 0) )) (< (/ indexX (float 8)) (- 8 1)))	 
			(setf leftDown (CheckMoveDL oth indexX endColor))
		)

		;Determine if the down right sucessor can be generated
		(when (and (< (mod indexX 8) (- 8 1)) (< (/ indexX (float 8)) (- 8 1)))
			
			(setf rightDown(CheckMoveDR oth indexX endColor))
			
		)

		(setf endPos (list left right up down leftUp rightUp leftDown rightDown))
		(when (and (< indexX 64)(> indexX -1)) 
			(when (not (eq (nth indexX oth) '-))
				(setf endPos nil)
			)
		)
		
		
		
;		(print lst)
;		(Print "-----")
		
;		(Print left) 
;		(Print right) 
;		(Print up) 
;		(Print down) 
;		(Print leftUp)
;		(Print rightUp)
;		(Print leftDown)
;		(Print rightDown)

		

		(setf lst(CreateMove oth pos endPos endColor))
		
		lst
	)
)

#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Parameters: 


Returns:


*****************************************************************************|#

(defun CheckMoveUL (oth pos color)
	(let (row)
	
		(setf endPos 0)
		
		(setf row (floor (/ pos 8)))
		
		(do ((indexX (- pos 9) (setf indexX (+ indexX '-9))))
					 ; did we find a position/       make sure we dont step off the edge
			((or (not (equal endPos 0))(< indexX 0))
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
				
	)
	endPos
)

#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Parameters: 


Returns:


*****************************************************************************|#

(defun CheckMoveUR (oth pos color)
	(let (row)
	
		(setf endPos 0)
		
		(setf row (floor (/ pos 8)))
		
		(do ((indexX (- pos 7) (setf indexX (+ indexX '-7))))
					 ; did we find a position/       make sure we dont step off the edge
			((or (not (equal endPos 0))(< indexX 0)))
			
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
				
	)
	endPos
)

#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Parameters: 


Returns:


*****************************************************************************|#

(defun CheckMoveDL (oth pos color)
	(let (row)
	
		(setf endPos 0)
		
		(setf row (floor (/ pos 8)))
		
		(do ((indexX (+ pos 7) (setf indexX (+ indexX '+7))))
					 ; did we find a position/       make sure we dont step off the edge
			((or (not (equal endPos 0))(> indexX 63))
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
				
	)
	endPos
)


#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Parameters: 


Returns:


*****************************************************************************|#

(defun CheckMoveDR (oth pos color)
	(let (row)
	
		(setf endPos 0)
		
		(setf row (floor (/ pos 8)))
		
		(do ((indexX (+ pos 9) (setf indexX (+ indexX '+9))))
					 ; did we find a position/       make sure we dont step off the edge
			((or (not (equal endPos 0))(> indexX 63))
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
				
	)
	endPos
)


#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Parameters: 


Returns:


*****************************************************************************|#

(defun CheckMoveD (oth pos color)
	(let (row)
	
		(setf endPos 0)
		
		(setf row (floor (/ pos 8)))
		
		(do ((indexX (+ pos 8) (setf indexX (+ indexX '+8))))
					 ; did we find a position
			((or (not (equal endPos 0))(> indexX 63)))
			
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
				
	)
	endPos
)


#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Parameters: 


Returns:


*****************************************************************************|#

(defun CheckMoveU (oth pos color)
	(let (row)
	
		(setf endPos 0)
		
		(setf row (floor (/ pos 8)))
		
		(do ((indexX (- pos 8) (setf indexX (+ indexX '-8))))
					 ; did we find a position
			((or (not (equal endPos 0)) (< indexX 0))
				)
			
			(when(and (not(equal indexX (- pos 8)))(equal (nth indexX oth) color))
				(setf endPos indexX)
			)
			
			(when(or (equal (nth indexX oth) '-) 
			(and (< indexX 0) (> indexX 63)))
				(setf endPos nil)
			)
			
			(when(and (equal indexX (- pos 8))(equal (nth indexX oth) color))
				(setf endPos nil)
			)
		)
				
	)
	endPos
)


#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Parameters: 


Returns:


*****************************************************************************|#

(defun CheckMoveR (oth pos color)
	(let (row)
	
		(setf endPos 0)
		
		(setf row (floor (/ pos 8)))
		
		(do ((indexX (+ pos 1) (setf indexX (+ indexX 1))))
					 ; did we find a position
			((or (not (equal endPos 0)))
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
				
	)
	endPos
)


#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Parameters: 


Returns:


*****************************************************************************|#

(defun CheckMoveL (oth pos color)
	(let (row)
	
		(setf endPos 0)
		
		(setf row (floor (/ pos 8)))
		
		(do ((indexX (- pos 1) (setf indexX (- indexX 1))))
					 ; did we find a positionge
			((or (not (equal endPos 0)) (< indexX 0))
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
				
	)
	endPos
)


#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Parameters: 


Returns:


*****************************************************************************|#

(defun CreateMove (lst pos endPos endColor)
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
;			(print nil)
		)
		
		(when (<= pos 0)
			(print "Move Made")
			(print pos)
		)
		
		tempList
	)
)


(defun AINoMove ()
	(if (null (move-generator *board* *AIColor*))
		nil
		t
	)

)

(defun PlayerNoMove ()
	(if (null (move-generator *board* *player*))
		nil
		t
	)

)

