(pl:transcode-to-string '*a-test-variable*)
(pl:transcode-to-string '1:10)
(pl:transcode-to-string :this-variable)
(pl:transcode-to-string "a test's test")
(pl:transcode-to-string '(quote this-test))
(pl:transcode-to-string '(setq some-variable (quote other-variable)))
(pl:transcode-to-string '(setq some-variable (quote other-variable)))
(pl:transcode-to-string '(progn x y z))
(pl:transcode-to-string '(if x y z))
(pl:transcode-to-string '(lambda (x y z) some-symbol))
(pl:transcode-to-string '(lambda (a b c) (progn a b some-symbol)))

(pl:transcode-to-string '(if (< x 10)
							 (progn 
							   (setq x 11)
							   x)
						   (progn 
							 (setq x 9)
							 x)))


(cl-prettyprint (macroexpand '(defun-match pl:transcode ((list 'if condition true false))
  "If is transcoded to a functional if."
  (insertf "fif(")
  (pl:transcode condition)
  (insertf ", @()")
  (pl:transcode true)
  (insertf ", @()")
  (pl:transcode false)
  (insertf ")"))))

(progn
  (extend-match-function 'pl:transcode
						 (lambda (defun-match-inner-args-11549)
						   (match1 (list (list 'if condition true false))
								   defun-match-inner-args-11549
								   "If is transcoded to a functional if."
								   (insertf "fif(")
								   (pl:transcode condition)
								   (insertf ", @()")
								   (pl:transcode true)
								   (insertf ", @()")
								   (pl:transcode false)
								   (insertf ")")))
						 '((list (quote if) condition true false))
						 "If is transcoded to a functional if.")
  (defalias 'pl:transcode
	(lambda (&rest defun-match-arg-list-11548)
	  (flet ((recur (&rest defun-match-inner-recur-args-11554)
					(list 'defun-match-recur-sigil-for-pl:transcode-7925
						  defun-match-inner-recur-args-11554)))
		(shadchen:let/named recur-point-11553
							((defun-match-possibles-11555 (gethash 'pl:transcode
																   *match-function-table*)))
							(cond ((not defun-match-possibles-11555)
								   (error "Match fail for matching defun %s with arguments %S."
										  'pl:transcode
										  defun-match-arg-list-11548))
								  (:otherwise (let* ((defun-match-fun-11550 (car defun-match-possibles-11555))
													 (defun-match-val-11551 (funcall defun-match-fun-11550
																					 defun-match-arg-list-11548)))
												(cond ((eq *match-fail*
														   defun-match-val-11551)
													   (recur-point-11553 (cdr defun-match-possibles-11555)))
													  ((and (listp defun-match-val-11551)
															(eq (car defun-match-val-11551)
																'defun-match-recur-sigil-for-pl:transcode-7925))
													   (setq defun-match-arg-list-11548 (cadr defun-match-val-11551))
													   (recur-point-11553 (gethash 'pl:transcode
																				   *match-function-table*)))
													  (:otherwise defun-match-val-11551))))))))
	(gethash 'pl:transcode *match-function-doc-table*)))
