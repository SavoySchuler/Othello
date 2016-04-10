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


;Not finished functions swp
;color - other players color
(defun GenerateSuccesors (oth color)
	(let (pos left right up down leftUp rightUp leftDown rightDown endColor childern)
		(if (eq color 'w)
			(setf endColor 'b)
			(setf endColor 'w)
		)
	
		(setf childern nil)
	
		(setf pos (AllPositions endColor oth))
	
		(dolist (indexX pos)
		(print indexX)
		;Determine if the left sucessor can be generated
		(when (and (> (mod indexX 8) 0)
			 (not (eq indexX 0) ))
			
			(when (eq (nth (- indexX 1) oth)'-)
				(CheckAllMoves oth (- indexX 1) color)
			)
			
		)

		(print "left")
		;Determine if the right sucessor can be generated
		(when (< (mod indexX 8) (- 8 1))
		
			(when (eq (nth (+ indexX 1) oth)'-)
					(setf right(CheckAllMoves oth (+ indexX 1) color))
			)
		
		)

		(print "right")
		;Determine if the up sucessor can be generated 
		(when (>= (/ indexX (float 8)) 1)
		
			(when (eq (nth (- indexX 8) oth)'-)
					(setf up (CheckAllMoves oth (- indexX 8) color))
			)
				
		)

		(print "up")
		;Determine if the down sucessor can be generated
		(when (< (/ indexX (float 8)) (- 8 1))
		
			(when (eq (nth (+ indexX 8) oth)'-)
					(setf down(CheckAllMoves oth (+ indexX 8) color))
			)
		)
		
		(print "down")
		;Determine if the up left sucessor can be generated
		(when (and (and (> (mod indexX 8) 0)
			(not (eq indexX 0) )) (>= (/ indexX (float 8)) 1))
			 
			(when (eq (nth (- indexX 9) oth)'-)
					(setf leftUp(CheckAllMoves oth (- indexX 9) color))
			)
		)

		(print "up left")
		;Determine if the up right sucessor can be generated
		(when (and (< (mod indexX 8) (- 8 1)) (>= (/ indexX (float 8)) 1))
			
			(when (eq (nth (- indexX 7) oth)'-)
					(setf rightUp(CheckAllMoves oth (- indexX 7) color))
			)

		)
		
		(print "up right")
		;Determine if the down left sucessor can be generated
		(when (and (and (> (mod indexX 8) 0)
			(not (eq indexX 0) )) (< (/ indexX (float 8)) (- 8 1)))
			(print "buttz")
			(print indexX)
			(when (eq (nth (+ indexX 7) oth)'-)
				(setf leftDown(CheckAllMoves oth (+ indexX 7) color))
			)

		)
		
		(print "down left")
		;Determine if the down right sucessor can be generated
		(when (and (< (mod indexX 8) (+ 8 1)) (< (/ indexX (float 8)) (- 8 1)))
			
			(when (eq (nth (+ indexX 9) oth)'-)
					(setf rightDown(CheckAllMoves oth (+ indexX 9) color))
			)
		)
		
		(print "down right")
		(setf childern (append childern (list left right up down leftUp rightUp leftDown rightDown)))
		
		)
		
;		(PrintOthello left) 
;		(PrintOthello right) 
;		(PrintOthello up) 
;		(PrintOthello down) 
;		(PrintOthello leftUp)
;		(PrintOthello rightUp)
;		(PrintOthello leftDown)
;		(PrintOthello rightDown)
		(setf childern(remove nil childern))
		(setf childern(remove-duplicates childern :test #'equal))
		
		(print "")
		(print "")
		(print "")
		(print "Printing Othello")
		(dolist (indexX childern)
			(PrintOthello indexX)
		)
		
		childern

	)
)



(defun AllPositions (color oth)
  (loop
    for element in oth 
    and position from 0
     when (eql element color)
	  collect position)

)


(defun CheckAllMoves (oth pos endColor)
(let (indexX lst left right up down leftUp rightUp leftDown rightDown)
;		(if (eq color 'w)
;			(setf endColor 'b)
;			(setf endColor 'w)
;		)
	
		;(setf pos (AllPositions color oth))
		(setf indexX pos)
		
		(print "1")
		;Determine if the left sucessor can be generated
		(when (and (> (mod indexX 8) 0)
			 (not (eq indexX 0) ))
			 
			(setf left (CheckMoveL oth indexX endColor))
		)
		
		;Determine if the right sucessor can be generated
		(when (< (mod indexX 8) (- 8 1))
			(setf right (CheckMoveR oth indexX endColor ))
		)
		(print "This is the problem")
		;Determine if the up sucessor can be generated 
		(when (>= (/ indexX (float 8)) 1)		
			(setf up (CheckMoveU oth indexX endColor ))
		)
		(print "Yup")
		;Determine if the down sucessor can be generated
		(when (< (/ indexX (float 8)) (- 8 1))
			(setf down (CheckMoveD oth indexX endColor))
		)
		
		;Determine if the up left sucessor can be generated
		(when (and (and (> (mod indexX 8) 0)
			(not (eq indexX 0) )) (>= (/ indexX (float 8)) 1))			 
			(setf leftUp (CheckMoveUL oth indexX endColor))
		)
		(print "2")
		;Determine if the up right sucessor can be generated
		(when (and (< (mod indexX 8) (- 8 1)) (>= (/ indexX (float 8)) 1))			
			(setf rightUp (CheckMoveUR oth indexX endColor))
		)
		(print "3")
		;Determine if the down left sucessor can be generated
		(when (and (and (> (mod indexX 8) 0)
			 (not (eq indexX 0) )) (< (/ indexX (float 8)) (- 8 1)))	 
			(setf leftDown (CheckMoveDL oth indexX endColor))
		)
		(print "4")
		;Determine if the down right sucessor can be generated
		(when (and (< (mod indexX 8) (- 8 1)) (< (/ indexX (float 8)) (- 8 1)))
			
			(setf rightDown(CheckMoveDR oth indexX endColor))
			
		)
		(print "5")
		(setf endPos (list left right up down leftUp rightUp leftDown rightDown))
		
		
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
		(CreateMove oth pos endPos)
	)
)



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
			
			(print "UGH")
		)
				
	)
	endPos
)

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

(defun CheckMoveL (oth pos color)
	(let (row)
	
		(setf endPos 0)
		
		(setf row (floor (/ pos 8)))
		
		(do ((indexX (- pos 1) (setf indexX (- indexX 1))))
					 ; did we find a positionge
			((or (not (equal endPos 0)))
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

(defun CreateMove (lst pos endPos)
	(let (tempList offsets endColor valid)
		(setf valid nil)
		(setf endColor 'b)
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
		
		(PrintOthello tempList)
		
		(when (null valid)
			(setf tempList nil)
			(print nil)
		)
		tempList
	)
)

#|
(defun human-move ()

(dolist (indexX pos)
	(let (pos left right up down leftUp rightUp leftDown rightDown endColor)
	
		(if (eq color 'w)
			(setf endColor 'b)
			(setf endColor 'w)
		)
		;Determine if the left sucessor can be generated
		(when (and (>= (mod indexX 8) 0)
			 (not (eq indexX 0) ))
			 
			;Insert Do that will check left
			 
			(setf endPos (CheckMove oth indexX endColor 1))
			 
			 (print "Left Step")
			 
			(setf left (CreateMove oth indexX -1 endPos endColor) )
		)

		(print "left")
		;Determine if the right sucessor can be generated
		(when (< (mod indexX 8) (- 8 1))
			(print "Right crawl")
			(CheckMove oth indexX endColor '-1)
		(if ( not(equal endPos nil)) )
			(print "Right step")
			
			(if (not (equal endPos nil))
				(setf right (CreateMove oth indexX 1 endPos endColor) )
			)
		)

		(print "right")
		;Determine if the up sucessor can be generated 
		(when (>= (/ indexX (float 8)) 1)
		
			(setf endPos(CheckMove oth indexX endColor '-8))
			
			(if (not (equal endPos nil))
			(setf up (CreateMove oth indexX 8 endPos endColor) )
			)
		)

		(print "up")
		;Determine if the down sucessor can be generated
		(when (< (/ indexX (float 8)) (- 8 1))
		
			(setf endPos (CheckMove oth indexX endColor 8))
			(format t "endPos up: ~S" endPos)
			(if (not (equal endPos nil))
				(setf down (CreateMove oth indexX '-8 endPos endColor) )
			)
		)
		
		(print "down")
		;Determine if the up left sucessor can be generated
		(when (and (and (>= (mod indeX 8) 0)
			(not (eq indxX 0) )) (>= (/ indexX (float 8)) 1))
			 
			(setf endPos (CheckMove oth indeX endColor 9))
			
			(if (not (equal endPos nil))
			(setf leftUp (CreateMove oth indexX '-9 endPos endColor))
			)
		)

		(print "up left")
		;Determine if the up right sucessor can be generated
		(when (and (< (mod indexX 8) (- 8 1)) (>= (/ indexX (float 8)) 1))
			
			(setf endPos (CheckMove oth indexX endColor 7))
			
			(if (not (equal endPos nil))
			(setf rightUp (CreateMove oth indexX '-7 endPos endColor))
			)

		)
		
		(print "up right")
		;Determine if the down left sucessor can be generated
		(when (and (and (>= (mod indexX 8) 0)
			 (not (eq indexX 0) )) (< (/ indexX (float 8)) (- 8 1)))
			 
			(setf endPos (CheckMove oth indexX endColor '-7))
			
			(if (not (equal endPos nil))
			(setf leftDown (CreateMove oth indexX 7 endPos endColor) )
			)

		)

		(print "down left")
		;Determine if the down right sucessor can be generated
		(when (and (< (mod indexX 8) (- 8 1)) (< (/ indexX (float 8)) (- 8 1)))
			
			(CheckMove oth indexX endColor '-9)
			
			(if (not (equal endPos nil))
			(setf rightDown (Swp state pos 9 ))
			)
		)
		
		(print "down right")
		
		)
		
	)
)


|#