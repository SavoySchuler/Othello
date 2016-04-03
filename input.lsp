(defun input(args)
	(print "input huehue")
	(let ((filename) (puzList))
		(cond
			((= (length args) 1)    
				(setf player (car args))	;Will want a set player function
					(format t "Would you like to move first [y/n]? ") 
					(setf firstPlayer (read)) 								
			)
			
			; else statement prints usage statement
			(t "Usage: clisp Othello.lsp <player - 'Black' or 'White'>")
		)	
		(cond
			((equal "Black" player)
			(setf player 'b)
			)
			
			((equal "White" player)
			(setf player 'w)
			)
		)
	)
)

|#|

Testing
(input *args*);function call
(print player)
(print firstPlayer)
|#|
