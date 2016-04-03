(defun input(args)
	(let ((filename) (puzList))
		(cond
			((= (length args) 1)    
				(cond
				
					((equal "Black" (car args))
						(setf player 'b)
					)
			
					((equal "White" (car args))
						(setf player 'w)
					)
				)	
			
				
				
				
				
				
				
				
				
				
				(format t "Would you like to move first [y/n]? ") 
				(setf firstPlayer (read)) 
				(cond

					((equal 'y firstPlayer)
						(setf firstPlayer 'y)
					)
			
					((equal 'n firstPlayer)
						(setf firstPlayer 'n)
					)
					
			(t (input (car args)))
				)	
				
				
				
				
				
				;(set firstPlayer (read)) 								
			
			)
			
			; else statement prints usage statement
			(t "Usage: clisp Othello.lsp <player - 'Black' or 'White'>")
		)	

	)
)



;Testing
(input *args*);function call
(print player)
(print firstPlayer)

