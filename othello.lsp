(load (merge-pathnames "input.lsp" *load-truename*))
(load (merge-pathnames "othello-init.lsp" *load-truename*))
(load (merge-pathnames "minimax.lsp" *load-truename*))
(load (merge-pathnames "move-generator.lsp" *load-truename*))
(load (merge-pathnames "heuristics.lsp" *load-truename*))

(defun othello (args)
	(input args)
	;(print *player*)	;TESTING
)

(defun test ()
;	(minimax *lst2* 2 *AIColor*)

	(setf butt (minimax *lst2* 2 *AIColor*) )
)

(othello *args*)



;know: (load 'othello)
;know: (test)
;know: (PrintOthello (nth 0 (nth 1 butt)))
