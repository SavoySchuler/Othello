#|
                  ***** MINIMAX.LSP *****

Generalized recursive minimax routine.

Author: Dr. John M. Weiss
Class:	SDSM&T CSC447/547 Artificial Intelligence
Date: 	Spring 2016

Usage:    (minimax position depth)
          where position is the position to be evaluated,
          and depth is the search depth (number of plys).

Returns:  (value path)
          where value is the backed-up value from evaluation of leaf nodes,
          and path is the path to the desired leaf node.

Functions called:

          (deepenough depth) -
              predicate that returns T if the current position has reached
              the desired search depth, NIL otherwise.

          (move-generator position) -
              generates successors to the position.

          (static position) -
              applies the static evaluation function to the position.

          Note: these functions may need additional arguments.
|#

(defun minimax (position depth color)

	

    ; if we have searched deep enough, or there are no successors,
    ; return position evaluation and nil for the path
    (if (or (deepenough depth) (null (move-generator position color)))
        (list (aroundCornerHueristic position color) nil)				;hueristic calc
;	(Print (cornerHueristic position color))
;	(PrintOthello position)

        ; otherwise, generate successors and run minimax recursively
        (let
            (
                ; generate list of sucessor positions
                (successors (move-generator position color))
		;(successors (GenerateSuccessors position))		;Name change
                
		; initialize current best path to nil
                (best-path nil)

                ; initialize current best score to negative infinity
                (best-score -1000000)

                ; other local variables
                succ-value
                succ-score
            )
		
            ; explore possible moves by looping through successor positions
            (dolist (successor successors)
		
		
                ; perform recursive DFS exploration of game tree
		
		(if (equal color 'w)
			(setq succ-value (minimax successor (1- depth) 'b))
			(setq succ-value (minimax successor (1- depth) 'w))
		)
		
                ; change sign every ply to reflect alternating selection
                ; of MAX/MIN player (maximum/minimum value)
                (setq succ-score (- (car succ-value)))
		
                ; update best value and path if a better move is found
                ; (note that path is being stored in reverse order)
                (when (> succ-score best-score)
                      (setq best-score succ-score)
                      (setq best-path (cons successor (cdr succ-value)))
                )
            )

            ; return (value path) list when done
            (list best-score best-path)
        )
    )
)

(defun deepenough (depth)
	(if (eq 0 depth) 
		t
		nil
	)
)

(defun static (position)
	;(cond  ((null position) nil)
   	;((equal 'b (car position))(+ 1 (static 'b (cdr position))))
    	;(t (static 'b(cdr position)))
	;)

;	1
)


(defun aroundCornerHueristic (lst color)
	(let (endColor sum)
		(setf sum 0)
		
		(if (equal color 'w)
			(setf enColor 'b)
			(setf enColor 'w)
		)
		
		;Check top left corner for each players piece
		(when (eq(nth 0 lst) color)
			(setf sum (+ sum 10))
		)
		
		(when (eq(nth 0 lst) enColor)
			(setf sum (- sum 10))
		)

		(when (eq(nth 7 lst) color)
			(setf sum (+ sum 10))
		)
		
		(when (eq(nth 7 lst) enColor)
			(setf sum (- sum 10))
		)
		
		(when (eq(nth 56 lst) color)
			(setf sum (+ sum 10))
		)
		
		(when (eq(nth 56 lst) enColor)
			(setf sum (- sum 10))
		)
		
		(when (eq(nth 63 lst) color)
			(setf sum (+ sum 10))
		)
		
		(when (eq(nth 63 lst) enColor)
			(setf sum (- sum 10))
		)
		sum
	)
)

(defun aroundCornerHueristic (lst color)
	(let (endColor sum)
		(setf sum 0)
		
		(if (eq color 'w)
			(setf enColor 'b)
			(setf enColor 'w)
		)
		
		;Check top left corner for each players piece
		(when (eq(nth 1 lst) color)
			(setf sum (- sum 10))
		)
		
		(when (eq(nth 1 lst) enColor)
			(setf sum (+ sum 10))
		)

		(when (eq(nth 8 lst) color)
			(setf sum (- sum 10))
		)
		
		(when (eq(nth 8 lst) enColor)
			(setf sum (+ sum 10))
		)
		
		(when (eq(nth 9 lst) color)
			(setf sum (- sum 10))
		)
		
		(when (eq(nth 9 lst) enColor)
			(setf sum (+ sum 10))
		)
		
		(when (eq(nth 48 lst) color)
			(setf sum (- sum 10))
		)
		
		(when (eq(nth 48 lst) enColor)
			(setf sum (+ sum 10))
		)
		
		(when (eq(nth 49 lst) color)
			(setf sum (- sum 10))
		)
		
		(when (eq(nth 49 lst) enColor)
			(setf sum (+ sum 10))
		)
		
		(when (eq(nth 57 lst) color)
			(setf sum (- sum 10))
		)
		
		(when (eq(nth 57 lst) enColor)
			(setf sum (+ sum 10))
		)
		
		(when (eq(nth 55 lst) color)
			(setf sum (- sum 10))
		)
		
		(when (eq(nth 55 lst) enColor)
			(setf sum (+ sum 10))
		)
		
		(when (eq(nth 54 lst) color)
			(setf sum (- sum 10))
		)
		
		(when (eq(nth 54 lst) enColor)
			(setf sum (+ sum 10))
		)
		
		(when (eq(nth 62 lst) color)
			(setf sum (- sum 10))
		)
		
		(when (eq(nth 62 lst) enColor)
			(setf sum (+ sum 10))
		)
		
		(setf sum (+ sum 23))
	)
)
