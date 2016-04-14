

(defun cornerHeuristic (lst color)
	(let (endColor sum)
		(setf sum 0)
		
		(if (equal color 'w)
			(setf enColor 'b)
			(setf enColor 'w)
		)
		
		;Check top left corner for each players piece
		(when (eq(nth 0 lst) color)
			(setf sum (+ sum 100))
		)
		
		(when (eq(nth 0 lst) enColor)
			(setf sum (- sum 100))
		)

		(when (eq(nth 7 lst) color)
			(setf sum (+ sum 100))
		)
		
		(when (eq(nth 7 lst) enColor)
			(setf sum (- sum 100))
		)
		
		(when (eq(nth 56 lst) color)
			(setf sum (+ sum 100))
		)
		
		(when (eq(nth 56 lst) enColor)
			(setf sum (- sum 100))
		)
		
		(when (eq(nth 63 lst) color)
			(setf sum (+ sum 100))
		)
		
		(when (eq(nth 63 lst) enColor)
			(setf sum (- sum 100))
		)
		sum
	)
)

(defun aroundCornerHeuristic (lst color)
	(let (endColor sum)
		(setf sum 0)
		
		(if (eq color 'w)
			(setf enColor 'b)
			(setf enColor 'w)
		)
		
		;Check top left corner for each players piece
		(when (eq(nth 1 lst) color)
			(setf sum (- sum 50))
		)
		
		(when (eq(nth 1 lst) enColor)
			(setf sum (+ sum 50))
		)

		(when (eq(nth 8 lst) color)
			(setf sum (- sum 50))
		)
		
		(when (eq(nth 8 lst) enColor)
			(setf sum (+ sum 50))
		)
		
		(when (eq(nth 9 lst) color)
			(setf sum (- sum 50))
		)
		
		(when (eq(nth 9 lst) enColor)
			(setf sum (+ sum 50))
		)
		
		(when (eq(nth 48 lst) color)
			(setf sum (- sum 50))
		)
		
		(when (eq(nth 48 lst) enColor)
			(setf sum (+ sum 50))
		)
		
		(when (eq(nth 49 lst) color)
			(setf sum (- sum 50))
		)
		
		(when (eq(nth 49 lst) enColor)
			(setf sum (+ sum 50))
		)
		
		(when (eq(nth 57 lst) color)
			(setf sum (- sum 50))
		)
		
		(when (eq(nth 57 lst) enColor)
			(setf sum (+ sum 50))
		)
		
		(when (eq(nth 55 lst) color)
			(setf sum (- sum 50))
		)
		
		(when (eq(nth 55 lst) enColor)
			(setf sum (+ sum 50))
		)
		
		(when (eq(nth 54 lst) color)
			(setf sum (- sum 50))
		)
		
		(when (eq(nth 54 lst) enColor)
			(setf sum (+ sum 50))
		)
		
		(when (eq(nth 62 lst) color)
			(setf sum (- sum 50))
		)
		
		(when (eq(nth 62 lst) enColor)
			(setf sum (+ sum 50))
		)
		
	sum
	)
)




(defun morePlayerCount (position color)
	(let (endColor sum)
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

(defun (position color)
	(let (lst sum)
		
		(setf sum 0)
		(setf lst (AllPositions position color))
		
		(dolist (indexX lst)
			(setf sum (+ sum (nth indexX *positionalStrat)))
		)
		
		sum
	)
)
