

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
		
		(setf sum (+ sum 23))						;Why do we add 23 here??
	)
)



#|
(defun static (position)
	(let (endColor sum)
		(setf sum 0)
		
		(if (equal color 'w)
			(setf enColor 'b)
			(setf enColor 'w)
		)	

	;(cond  ((null position) nil)
   	;((equal 'b (car position))(+ 1 (static 'b (cdr position))))
    	;(t (static 'b(cdr position)))
	;)

;	1
)
|#


