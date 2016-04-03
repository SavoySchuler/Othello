(defun input(args)
	(let ((filename) (puzList))
		(cond
			((= (length args) 1)    
				(setf player (merge-pathnames (car args) *load-truename*))		;Will want a set player function
					("Would you like to move first [y/n]?")
					(setf firstPlayer (merge-pathnames (car args) *load-truename*))	;Will want a set first player function
			)
			
			; else statement prints usage statement
			(t "Usage: clisp Othello.lsp <player - 'Black' or 'White'>")
		)	
	)
)




