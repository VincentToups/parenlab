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
(pl:transcode-to-string '(: a c))
(pl:transcode-to-string '(cell #'sin))
(pl:transcode-to-string [1 2 3 4])
(match (list [1 2 3])  
	   ((list (p #'vectorp v))  v))

(defun-match- test ((p #'vectorp v)) v)
(test [1 2])

(pl:transcode [1 2 | 3 4])


(pl:transcode-to-string '(if (< x 10)
							 (progn 
							   (setq x 11)
							   x)
						   (progn 
							 (setq x 9)
							 x)))

(pl:transcode-sequence '((setq x 10) (setq y 11) (+ x y)))

(pl:transcode '(function (a b c) test-function (x y)
						 "This is a doc string."
						 (setq a (+ x y))
						 (setq b (+ x a))
						 (setq c (+ x b))))


(pl:transcode-to-string '(with x 10 (+ x 1)))

(pl:transcode-to-string '(if (< x 10) (block (setq x 1000))
						   (block (setq y 1000))))

(pl:transcode-to-string '(for x 1:10 (disp x)))
