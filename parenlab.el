(require 'shadchen)

(setq max-lisp-eval-depth 1000)

(defun pl:symbol-with-indexing-syntax (x)
  "Return T when x is a non-keyword symbol with indexing syntax."
  (and (symbolp x)
	   (not (keywordp x))
	   (let* ((parts (split-string (symbol-name x) ":"))
			  (n (length parts)))
		 (or (= n 2)
			 (= n 3)))))

(defun pl:non-keyword-symbolp (x)
  "T when X is a symbol but not a keyword."
  (and (symbolp x)
	   (not (keywordp x))))

(defun pl:insertf (fs &rest args)
  (insert (apply #'format fs args)))

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

(defun pl:=/c (a)
  (eval `(lambda (b) (= ,a b))))

(defun pl:read-from-string (s)
  (match (read-from-string s)
		 ((cons r (p (pl:=/c (length s))))
		  r)))

(defun-match- pl:transcode (nil)
  "Nil is transcoded to an empty array."
  (pl:insertf "[]"))


(defun-match pl:transcode ((p #'pl:symbol-with-indexing-syntax s))
  "Symbols are mangled and inserted as variables."
  (match (split-string (symbol-name s) ":")
		 ((list start stop)
		  (pl:insertf "((")
		  (pl:transcode (pl:read-from-string start))
		  (pl:insertf "):(")
		  (pl:transcode (pl:read-from-string stop))
		  (pl:insertf "))"))
		 ((list start step stop)
		  (pl:insertf "((")
		  (pl:transcode (pl:read-from-string start))
		  (pl:insertf "):(")
		  (pl:transcode (pl:read-from-string step))
		  (pl:insertf "):(")
		  (pl:transcode (pl:read-from-string stop))
		  (pl:insertf "))"))))

(defun-match pl:transcode ((p #'vectorp v))
  "Handle vector."
  (let ((start (point)))
	(pl:insertf "[ ")
	(loop for item across v do
		  (if (eq '| item) 
			  (pl:insertf ";\n")
			(progn (pl:transcode item)
				   (pl:insertf " "))))
	(pl:insertf "]")
	(indent-region start (point))))


(defun-match pl:transcode ((p #'pl:non-keyword-symbolp s))
  "Symbols are mangled and inserted as variables."
  (pl:insertf "%s" (pl:mangle s)))

(defun-match pl:transcode ((p #'numberp n))
  "Numbers are inserted as is."
  (pl:insertf "%s" n))


(defun-match pl:transcode ((p #'keywordp k))
  "Keywords are transcoded as symbols but into strings."
  (pl:insertf "'%s'" (pl:mangle (substring (symbol-name k) 1))))

(defun pl:escape-single-quotes (s)
  (replace-regexp-in-string "'" "''" s))

(defun-match pl:transcode ((p #'stringp s))
  "Strings are transcoded to matlab strings, escaping single quotes."
  (pl:insertf "'%s'" (pl:escape-single-quotes s)))

(defun-match pl:transcode ((list 'quote form))
  "Quotations are evaluated to strings whose value under eval is
  the value of the forms."
  (pl:transcode (pl:transcode-to-string form)))

(defun-match pl:transcode ((list 'not form))
  "Translate the not operator."
  (pl:insertf "~(")
  (pl:transcode form)
  (pl:insertf ")"))

(defun-match pl:transcode ((list 'setq 
								 target 
								 value))
  "Set is transcoded to assignment."
  (pl:transcode target)
  (pl:insertf " = ")
  (pl:transcode value))

(defun-match pl:transcode ((list-rest 'progn forms))
  "Progn is transcoded to a hack function call."
  (pl:insertf "progn(")
  (loop for form in forms do
		(let ((string (pl:transcode-to-string form)))
		  (pl:transcode string))
		(pl:insertf ", "))
  (delete-region (point) (- (point) 2))
  (pl:insertf ")"))

(defun-match pl:transcode ((list-rest 'block body))
  "A block is inserted directly into the code as a sequence of
  operations."
  (pl:transcode-sequence body))

(defun-match pl:transcode ((list 'if condition 
								 (list-rest 'block true-body)
								 (list-rest 'block false-body)))
  "When if is invoked with block legs it is translated into a
regular, non-functional if statement."
  (let ((start (point)))
	(pl:insertf "if ")
	(pl:transcode condition)
	(pl:insertf "\n")
	(pl:transcode-sequence true-body)
	(pl:insertf "else\n")
	(pl:transcode-sequence false-body)
	(pl:insertf "end\n")
	(indent-region start (point))))


(defun-match pl:transcode ((list ': first last))
  "Transcode short array creation."
  (pl:insertf "((")
  (pl:transcode first)
  (pl:insertf "):(")
  (pl:transcode last)
  (pl:insertf "))"))

(defun-match pl:transcode ((list ': first step last))
  "Transcode short array creation."
  (pl:insertf "((")
  (pl:transcode first)
  (pl:insertf "):(")
  (pl:transcode step)
  (pl:insertf "):(")
  (pl:transcode last)
  (pl:insertf "))"))

(defun-match pl:transcode ((list-rest '{} variable indexes))
  "Encode a cell array access."
  (pl:transcode variable)
  (pl:insertf "{")
  (loop for index in indexes 
		and i from 1 do
		(pl:transcode index)
		when (< i (length indexes))
		do (pl:insertf ", "))
  (pl:insertf "}"))

(defun-match pl:transcode ((list 'if condition 
								 (list-rest 'block true-body)))
  "When if is invoked with block legs it is translated into a
regular, non-functional if statement."
  (let ((start (point)))
	(pl:insertf "if ")
	(pl:transcode condition)
	(pl:insertf "\n")
	(pl:transcode-sequence true-body)
	(pl:insertf "end\n")))

(defun-match pl:transcode ((list 'if condition true false))
  "If is transcoded to a functional if."
  (pl:insertf "fif(")
  (pl:transcode condition)
  (pl:insertf ", @()")
  (pl:transcode true)
  (pl:insertf ", @()")
  (pl:transcode false)
  (pl:insertf ")"))

(defun-match pl:transcode ((list 'lambda (p #'listp args) form))
  "Lambda is transcoded to a regular @ lambda."
  (pl:insertf "@(")
  (loop for arg in args 
		and i from 0 do
		(pl:transcode arg)
		when (< i (- (length args) 1))
		do (pl:insertf ", "))
  (pl:insertf ")")
  (pl:transcode form))

(defun-match pl:transcode ((list-rest 'for (p #'symbolp v) expr body))
  "Expand a for loop."
  (let ((start (point)))
	(pl:insertf "for ")
	(pl:transcode v)
	(pl:insertf " = ")
	(pl:transcode expr)
	(pl:insertf "\n")
	(pl:transcode-sequence body)
	(pl:insertf "end\n")
	(indent-region start (point))))

(defun-match pl:transcode ((list 'literally string))
  "Allows you to insert code directly into the output stream."
  (pl:insertf string))

(defun-match pl:transcode ((list-rest 'while condition body))
  "Encode a while loop."
  (let ((start (point)))
	(pl:insertf "while ")
	(pl:transcode condition)
	(pl:insertf "\n")
	(pl:transcode-sequence body)
	(pl:insertf "end\n")
	(indent-region start (point))))

(defun-match pl:transcode ((list 'function (p #'symbolp s)))
  "Encode a function namespace query."
  (pl:insertf "@")
  (pl:transcode s))

(defun-match pl:transcode ((list-rest 'defun
									  (pl:arglist outargs)
									  (p #'symbolp name) 
									  (pl:arglist inargs) 
									  body))
  "Transcode a function into an m-file."
  (let ((output-buffer (find-file-noselect (concat (pl:mangle (symbol-name name)) ".m"))))
	(with-current-buffer output-buffer
	  (delete-region (point-min)
					 (point-max))
	  (pl:insertf "function [")
	  (loop for o in outargs 
			and i from 1
			do 
			(pl:transcode o)
			when (< i (length outargs))
			do (pl:insertf ", "))
	  (pl:insertf "] = ")
	  (pl:transcode name)
	  (pl:insertf "(")
	  (loop for a in inargs
			and i from 1 do
			(pl:transcode a)
			when (< i (length inargs))
			do
			(pl:insertf ", "))
	  (pl:insertf ")\n")
	  (when (stringp (car body))
		(pl:insertf "%s\n" (pl:fix-comment-string (car body))))
	  (pl:transcode-sequence body)
	  (basic-save-buffer))
	(kill-buffer output-buffer)))

(defun-match pl:transcode ((list-rest 'script name body))
  "Encode a body into a script."
  (let ((output-buffer (find-file-noselect (concat (pl:mangle (symbol-name name)) ".m"))))
	(with-current-buffer output-buffer
	  (delete-region (point-min)
					 (point-max))
	  (pl:transcode-sequence body)
	  (basic-save-buffer))
	(kill-buffer output-buffer)))

(defvar *pl-macros* (make-hash-table))

(defun pl:pl-macrop (symbol)
  "Check if symbol is associated with a pl-macro."
  (and (symbolp symbol)
	   (gethash symbol *pl-macros*)))

(defmacro* pl:def-pl-macro (name lamlist &body body)
  (let ((args (gensym "args-")))
	`(setf (gethash ',name *pl-macros*)
		   (lambda (&rest ,args)
			 (destructuring-bind ,lamlist ,args ,@body)))))

(defun-match pl:transcode ((list-rest (p #'pl:pl-macrop dispatch) arguments))
  "Handle the expansion of pl-macros."
  (pl:transcode (apply (pl:pl-macrop dispatch) arguments)))

(pl:def-pl-macro with (symbol value &body body)
				 `(funcall (lambda (,symbol) ,@body) ,value))

(defun-match pl:transcode ((list-rest (p #'pl:non-keyword-symbolp the-function) arguments))
  "Handle the-function calls."
  (pl:transcode the-function)
  (pl:insertf "(")
  (loop for arg in arguments and
		i from 1 
		do 
		(pl:transcode arg)
		when (< i (length arguments))
		do
		(pl:insertf ", "))
  (pl:insertf ")"))

(defun-match- pl:maybe-empty-list-of-symbols (nil) t)
(defun-match pl:maybe-empty-list-of-symbols ((and (p #'listp)
												  (cons item rest)))
  "Returns t when input is a list of symbols."
  (if (symbolp item)
	  (recur rest)
	nil))

(defpattern pl:arglist (&optional pattern)
  (if (not pattern) `(p #'pl:maybe-empty-list-of-symbols)
	`(p #'pl:maybe-empty-list-of-symbols ,pattern)))

(defun pl:fix-comment-string (s)
  (concat "%" (replace-string-in-string (format "\n")
										(concat (format "\n") "%")
										s)))

(defun-match- pl:transcode-sequence (nil) 
  "Don't do anything with the empty sequence."
  nil)
(defun-match  pl:transcode-sequence ((list-rest form forms))
  "Encode an element of a sequence and encode the rest."
  (pl:transcode form)
  (pl:insertf ";\n")
  (recur forms))

(defmacro* pl:defun (oargs name iargs &body body)
  "Define a matlab function by creating a file and filling it in
with the transcoded code."
  `(pl:transcode '(defun ,oargs ,name ,iargs ,@body)))


(pl:defun (r) ++ (varargin)
		  "What?"
		  (setq r 0) 
		  (for x varargin
			   (setq r (+ r ({} x 1)))))

(pl:defun (r) ** (varargin)
		  "What?"
		  (setq r 1) 
		  (for x varargin
			   (setq r (* r ({} x 1)))))
