(load (merge-pathnames "input.lsp" *load-truename*))
(load (merge-pathnames "othello-init.lsp" *load-truename*))
(load (merge-pathnames "minimax.lsp" *load-truename*))
(load (merge-pathnames "gen-successors" *load-truename*))

(defun othello (args)
	(input args)
	;(print *player*)	;TESTING
)

(defun test ()
	(minimax *lst2* 2)
)

(othello *args*)
