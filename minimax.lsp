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

          (static position color) -
              applies the static evaluation function to the position.

          Note: these functions may need additional arguments.
|#

(defun minimax (position depth color alpha beta isMaxLevel) 

    ; if we have searched deep enough, or there are no successors,
    ; return position evaluation and nil for the path
    (if (or (deepenough depth) (null (move-generator position color)))
        (list (static position color) nil) ;Heuristic calc

        ; otherwise, generate successors and run minimax recursively
        
		;Local Variables:
		(let
            (
                ; generate list of sucessor positions
                (successors (move-generator position color))
		

				;(successors (GenerateSuccessors position))		;Name change?	;Do we need this now?	
                
				; initialize current best path to nil
                (best-path nil)

                ; initialize current best score to negative infinity
                (best-score -1000000)

                ; other local variables
                succ-value
                succ-path			
            )


            ; explore possible moves by looping through successor positions
            (dolist (successor successors)

                ; perform recursive DFS exploration of game tree
                (cond	
						((equal color 'w)
							(setq succ-value (minimax successor (1- depth) 'b alpha beta nil))

						)

						((equal color 'b)
							(setq succ-value (minimax successor (1- depth) 'w alpha beta nil))
						)

					)


                ; change sign every ply to reflect alternating selection
                ; of MAX/MIN player (maximum/minimum value)
                (setq succ-score (- (car succ-value)))


		(if (eq isMaxLevel t)
			(if (> succ-score alpha)
				(setf alpha succ-score)	
			)
			(if (< beta succ-score)
				(setf beta succ-score)
			)
		)

		(if (<= beta alpha)
			(return)
		)

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

#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Parameters: 


Returns:


*****************************************************************************|#

(defun deepenough (depth)
	(if (>= 0 depth) 
		t
		nil
	)
)


#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function:		

Description: 		


Parameters: 


Returns:


*****************************************************************************|#

(defun static (position color)
	(let (sum numBlanks)
	
	(cond	
		((equal color 'w)
		(setq color 'b))

		((equal color 'w)
		(setq color 'b))		
	)
	(setf sum 0)
	(setf sumBlanks 0)
	(setf sum (+ sum (posStrat position color)))
;	(setf sum (+ sum (isBetween color position *edgeTopRow* 1)))
;	(setf sum (+ sum (isBetween color position *edgeBottomRow* 1)))
;	(setf sum (+ sum (isBetween color position *edgeLeftColumn* 8)))
;	(setf sum (+ sum (isBetween color position *edgeRightColumn* 8)))
	
	(setf numBlanks 0)	

	;find out how many blank spaces remain
	(dolist (tilePiece *board*)  
	 	(when (equal tilePiece '-) 
			(setf numBlanks (+ numBlanks 1))
		)
	)

	;heuristic to play for less pieces early on, then reverse
	(if (< sumBlanks 25)
		(setf sum (+ sum (morePlayerCount position color)))
		(setf sum (+ sum (moreOpponentCount position color)))
	)

	sum	
	)
)
