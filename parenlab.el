(require 'shadchen)

(defun pl:non-keyword-symbolp (x)
  "T when X is a symbol but not a keyword."
  (and (symbolp x)
	   (not (keywordp x))))

(defun pl:mangle (s)
  (let* ((s (if (symbolp s) (symbol-name s) s))
		 (s1 (replace-regexp-in-string "-\\([a-z]\\)" 
									   (lambda (x)
										 (upcase (substring x 1))) 
									   s)))
	(replace-regexp-in-string 
	 (rx 
	  (| "+" "-" "*" "$" "!" ":" "/" "\\" "#" "@" "?" "="
		 "<" ">"))
	 (match-lambda 
	  ("+" "plus")
	  ("-" "minus")
	  ("*" "mtimes")
	  ("<" "lessThan")
	  (">" "greaterThan")
	  ("$" "cash")
	  ("=" "equal")
	  ("!" "bang")
	  ("?" "who")
	  (":" ":")
	  ("/" "divide")
	  ("\\" "mdivide")
	  ("#" "hash")
	  ("@" "at"))
	 s1)))

(defun-match- pl:transcode-to-string (form)
  (with-temp-buffer 
	(pl:transcode form)
	(buffer-substring (point-min)
					  (point-max))))

(defun-match- pl:transcode ((p #'pl:non-keyword-symbolp s))
  "Symbols are mangled and inserted as variables."
  (insertf "%s" (pl:mangle s)))

(defun-match pl:transcode ((p #'numberp n))
  "Numbers are inserted as is."
  (insertf "%s" n))

(defun-match pl:transcode ((p #'keywordp k))
  "Keywords are transcoded as symbols but into strings."
  (insertf "'%s'" (pl:mangle (substring (symbol-name k) 1))))

(defun pl:escape-single-quotes (s)
  (replace-regexp-in-string "'" "''" s))

(defun-match pl:transcode ((p #'stringp s))
  "Strings are transcoded to matlab strings, escaping single quotes."
  (insertf "'%s'" (pl:escape-single-quotes s)))

(defun-match pl:transcode ((list 'quote form))
  "Quotations are evaluated to strings whose value under eval is
  the value of the forms."
  (pl:transcode (pl:transcode-to-string form)))

(defun-match pl:transcode ((list 'setq 
								 (p #'symbolp target) 
								 value))
  "Set is transcoded to assignment."
  (pl:transcode target)
  (insertf " = ")
  (pl:transcode value))

(defun-match pl:transcode ((list-rest 'progn forms))
  "Progn is transcoded to a hack function call."
  (insertf "progn(")
  (loop for form in forms do
		(let ((string (pl:transcode-to-string form)))
		  (pl:transcode string))
		(insertf ", "))
  (delete-region (point) (- (point) 2))
  (insertf ")"))

(defun-match pl:transcode ((list 'if condition true false))
  "If is transcoded to a functional if."
  (insertf "fif(")
  (pl:transcode condition)
  (insertf ", @()")
  (pl:transcode true)
  (insertf ", @()")
  (pl:transcode false)
  (insertf ")"))

(defun-match pl:transcode ((list 'lambda (p #'listp args) form))
  "Lambda is transcoded to a regular @ lambda."
  (insertf "@(")
  (loop for arg in args 
		and i from 0 do
		(pl:transcode arg)
		when (< i (- (length args) 1))
		do (insertf ", "))
  (insertf ")")
  (pl:transcode form))

(defvar *pl-macros* (make-hash-table))

(defun pl:pl-macrop (symbol)
  "Check if symbol is associated with a pl-macro."
  (and (symbolp symbol)
	   (gethash symbol *pl-macros*)))

(defun-match pl:transcode ((list-rest (p #'pl:pl-macrop dispatch) arguments))
  "Handle the expansion of pl-macros."
  (pl:transcode (apply (pl:pl-macrop dispatch) arguments)))

(defun-match pl:transcode ((list-rest (p #'pl:non-keyword-symbolp the-function) arguments))
  "Handle the-function calls."
  (pl:transcode the-function)
  (insertf "(")
  (loop for arg in arguments and
		i from 1 
		do 
		(pl:transcode arg)
		when (< i (length arguments))
		do
		(insertf ", "))
  (insertf ")"))




