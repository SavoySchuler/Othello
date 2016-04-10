(load (merge-pathnames "input.lsp" *load-truename*))
(load (merge-pathnames "othello-init.lsp" *load-truename*))


(defun othello (args)
	(input args)
	;(print *player*)	;TESTING
)


(othello *args*)
