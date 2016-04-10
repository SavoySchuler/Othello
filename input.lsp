(defvar *player* NIL)

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
)

#|
;Testing
(input *args*);function call
(print *player*)
|#
