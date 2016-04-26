#|*****************************************************************************
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


*******************************************************************************

Modifications: Alex Nienheuser, Savoy Schuler 

Modification description:

	Dr. Weiss' minimax function was modified to include alpha-beta pruning to
	enhance time and efficiency. The alpha-beta pruning additions can be found
	mostly in the second half of the function. Additionally, function calls were
	modified to include extra paramaters added for calling minimax or other
	functions that minimax is dependent on but were not were not provided.

Modified Usage:
	
	(minimax position depth color alpha beta isMaxLevel) 

	Where position is the position to be evaluated, depth is the search depth 
	(number of plys), color is the color currently being assessed in minimax,
	alpha and beta are used for alpha-beta pruning and are passed in 
	from the last minimax call to determine upward/rightward position tree
	pruning, and isMaxLevel is a flag to tell whethere the alpha-beta pruning
	will be at a min level or a max level for current pass.

Added functions called:

	(minimax position depth color alpha beta isMaxLevel) - To recursively 
		evaluate a patheway to the most optimal move choice. 

*****************************************************************************|#

(defun minimax (position depth color alpha beta isMaxLevel) 

    ; if we have searched deep enough, or there are no successors,
    ; return position evaluation and nil for the path
    (if (or (deepenough depth) (null (move-generator position color)))
        (list (static position color) nil)  ;Heuristic calc

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

Function:	deepenough

Description: 
	
	This function is essentially the interator for minimax each time it is
	called. minimax() will decrement depth each iteration and pass it to
	deepenough(). The function will check to see if the depth is equal to or
	less than zero, and if so return t. Else it will return nil. This flag, if
	t, will tell minimax to stop expanding and return the present position 
	states, else it will keep expanding. 


Usage:	(deepenough depth)

	Where depth is the current number of times left to perform minimax.

Returns:	(t) or (nil)

	Where t if the specified depth value for the minimax has been met or nil if
	the specified depth value has not been met.

Functions called: none

*****************************************************************************|#

(defun deepenough (depth)
	(if (>= 0 depth) 
		t
		nil
	)
)


#|*****************************************************************************  
Author: Alex Nienheuser, Savoy Schuler

Function: static

Description: 		

	The static function is used to determine the value of a move by calculating
	the heurstical advantage of board state. The function performs each
	heuristical calculation and returns the added sum of all the hueristics. The
	primary heuristic is to weight each position by its staticstically determin-
	ed values, all of which are saved in a look-up table for fast access. 
	
	The secondary strategy is premised around the observation that it is more
	advantageous to play for less pieces early on. This allows the opposing
	player to fill center of the board with their pieces, allowing the AI to
	to take more advantageous pieces on the edges and outer region, each of 
	which will score significantly more pieces in the last stretch of the game,
	as the centric (and flipable) tiles will belong mostly to the opponent.
	This heuristic is enforced by valuing having less of your own pieces early
	on, but does not negate the value of a position as to not override the
	motivation to take corner and edge pieces. The switch as to when to start
	playing for more of the AI's own pieces is determined by the number of blank
	tiles remaining as to allow for easy adjustment for fine tuning. 

	It is noted that the secondary strategy is risky if an opponent has a keen
	eye for forcing the AI into moveless traps early. This consideration is
	solved by prefering positional value discontiguously with the secondary
	heuristic.  


Usage:    (static position color)
         
	Where position is a board state on which to perform heuristical calculations
	of its value and color is the current player's color. 

Returns:  (sum)

	Where sum is the the heuristically determined value of the board state to 
	the current player in calculation (alternates by ply)

Functions called:

	(position-strategy) - heuristic call for the value of a positions

	(more-player-count position color) - heuristic prefering more player pieces
	
	(more-opponent-count position color) - heurisitc prefering more opponent
		pieces.

*****************************************************************************|#

(defun static (position color)
	(let (sum numBlanks)
	
	;switch the color to represent the player who will have the next turn
	(cond	
		((equal color 'w)
		(setq color 'b))

		((equal color 'w)
		(setq color 'b))		
	)

	;initialize sums to zero
	(setf sum 0)
	(setf sumBlanks 0)
	
	;keep running sum of added value of each heuristic
	(setf sum (+ sum (position-strategy position color)))
	(setf sum (+ sum (is-between color position *edgeTopRow* 1)))
	(setf sum (+ sum (is-between color position *edgeBottomRow* 1)))
	(setf sum (+ sum (is-between color position *edgeLeftColumn* 8)))
	(setf sum (+ sum (is-between color position *edgeRightColumn* 8)))
	

	;find out how many blank spaces remain	
	(setf numBlanks 0)	
	(dolist (tilePiece *board*)  
	 	(when (equal tilePiece '-) 
			(setf numBlanks (+ numBlanks 1))
		)
	)

	;heuristic to play for less pieces early on, then reverse
	(if (< sumBlanks 25)
		(setf sum (+ sum (more-player-count position color)))
		(setf sum (+ sum (more-opponent-count position color)))
	)

	;return sum
;	(if (null sum)
;		(- 0 sum)
;		sum
;	)
	sum
	)
)
