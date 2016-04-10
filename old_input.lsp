(defvar *player* NIL)
(defvar *firstPlayer* NIL) 

(defun input(args)
	(let ((filename) (puzList))
			(when(= (length args) 1)    
				
				
					(when (equalp "Black" (car args))
						(setf *player* 'b)
						(firstMove)
					)
			
					(when (equalp "White" (car args))
						(setf *player* 'w)
						(firstMove)
					)

					(when (equalp "b" (car args))
						(setf *player* 'b)
						(firstMove)					
					)
			
					(when (equalp "w" (car args))
						(setf *player* 'w)
						(firstMove)
					)
					
					(when (null *player*)
						(print "Usage: clisp Othello.lsp <player - 'Black' or 'White'>") 
					)						
			)
	)
)	
	

				
(defun firstMove ()
	(format t "Would you like to move first [y/n]? ") 
	(let (temp)	(setf temp (read))
	
	(when (equalp 'y temp)
		(setf *firstPlayer* 'y)
	)
			
	(when (equalp 'n temp)
		(setf *firstPlayer* 'n)
	)
					
	(when (null *firstPlayer*) 
		(print "Please enter 'y' or 'n'.")
		(firstMove)
	)							
	
	)				
)
	
#|
;Testing
(input *args*);function call
(print *player*)
(print *firstPlayer*)
|#

