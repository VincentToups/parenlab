(require 'shadchen)

(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 2000)

(defvar pl:transplant-scripts-and-functions t)

(defvar pl:disable-indentation nil)

(defun pl:indent-region (s e)
  (unless pl:disable-indentation 
	(condition-case info 
		(indent-region s e)
	  (error nil))))

(defun pl:symbol-with-indexing-syntax (x)
  "Return T when x is a non-keyword symbol with indexing syntax."
  (and (symbolp x)
	   (not (keywordp x))
	   (let* ((parts (split-string (symbol-name x) ":"))
			  (n (length parts)))
		 (or (= n 2)
			 (= n 3)))))

(defvar pl:kill-opened-transcode-buffers nil)
(defun* pl:maybe-kill-buffer (&optional (buffer (current-buffer)))
  (if pl:kill-opened-transcode-buffers 
	  (kill-buffer buffer)
	nil))

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
									   s))
		 (s1 (replace-regexp-in-string (regexp-quote "%")
									   "modsign"
									   s1))
		 (s1 (if (string= "." (substring s1 0 1))
				 (concat "dot" (substring s1 1))
			   s1)))
	(replace-regexp-in-string 
	 (rx 
	  (| "~" "+" "-" "*" "%" "$" "&" "^" "!" ":" "/" "\\" "#" "@" "?" "="
		 "<" ">"))
	 (match-lambda 
	  ("+" "plus")
	  ("-" "minus")
	  ("*" "mtimes")
	  ("<" "lessThan")
	  (">" "greaterThan")
	  ("$" "cash")
	  ("=" "equal")
	  ("%" "modsign")
	  ("!" "bang")
	  ("?" "who")
	  (":" ":")
	  ("&" "ampersand")
	  ("^" "caret")
	  ("/" "divide")
	  ("\\" "mdivide")
	  ("#" "hash")
	  ("~" "tilda")
	  ("@" "at"))
	 s1)))

(defun-match- pl:transcode-to-string (form)
  (with-temp-buffer 
	(matlab-mode)
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

(defun-match pl:transcode ('return)
  "Return is transcoded as return."
  (pl:insertf "return"))

(defun-match pl:transcode ((or : (list :)))
  "Insert a naked :"
  (pl:insertf ":"))

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
	(pl:indent-region start (point))))


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

(defun-match pl:transcode ((list 'global (p #'symbolp name)))
  "Transcodes a global definition."
  (pl:insertf "global ")
  (pl:transcode name))

(defun-match pl:transcode ((list 'quote form))
  "Quotations are evaluated to strings whose value under eval is
  the value of the forms."
  (pl:transcode (let ((pl:disable-indentation t))
				  (pl:transcode-to-string form))))

(defun-match pl:transcode ((list-rest 'elisp forms))
  "Escape and execute lisp FORMS."
  (eval `(progn ,@forms)))

(defun-match pl:transcode ((list 'not form))
  "Translate the not operator."
  (pl:insertf "~(")
  (pl:transcode form)
  (pl:insertf ")"))

(defun-match pl:transcode ((list (or 'setq :=) 
								 target 
								 value))
  "Set is transcoded to assignment."
  (pl:transcode target)
  (pl:insertf " = ")
  (pl:transcode value :as-expression))

(defun pl:not-empty (l)
  "True when l is a non-empty list."
  (and (listp l)
	   (not (null l))))

(defun-match pl:transcode ((list-rest (or 'setq :=) 
									  target 
									  value 
									  (p #'pl:not-empty others)))
  "Set is transcoded to assignment."
  (pl:transcode target)
  (pl:insertf " = ")
  (pl:transcode value :as-expression)
  (pl:insertf ";\n")
  (recur `(setq ,@others)))

(defun-match pl:transcode ((list 'progn form))
  "Progn with a single form is the form itself."
  (pl:transcode form))

(defun-match pl:transcode ((list-rest 'progn forms))
  "Progn is transcoded to a hack function call."
  (pl:insertf "progn(")
  (loop for form in forms do
		(pl:transcode form :as-expression) 
		(pl:insertf ", "))
  (delete-region (point) (- (point) 2))
  (pl:insertf ")"))

(defun-match pl:transcode ((list-rest 'block body))
  "A block is inserted directly into the code as a sequence of
  operations."
  (let ((start (point-min)))
	(pl:transcode-sequence body)
	(pl:indent-region start (point-max))))

(defun-match pl:transcode ((list 'if condition 
								 (list-rest 'block true-body)
								 (list-rest 'block false-body)))
  "When if is invoked with block legs it is translated into a
regular, non-functional if statement."
  (let ((start (point)))
	(pl:insertf "if ")
	(pl:transcode condition)
	(pl:insertf "\n")
	(if (null true-body)
		(pl:transcode-sequence (list nil))
	  (pl:transcode-sequence true-body))
	(pl:insertf "else\n")
	(if (null false-body)
		(pl:transcode-sequence (list nil))
	  (pl:transcode-sequence false-body))
	(pl:insertf "end\n")
	(pl:indent-region start (point))))


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

(defun-match pl:transcode ((list '.. variable field))
  "Encode field access/setting."
  (pl:transcode variable)
  (pl:insertf ".(")
  (pl:transcode field)
  (pl:insertf ")"))

(defun-match pl:transcode ((list '.. variable (list-rest indexes) field))
  "Encode field access/setting."
  (pl:transcode variable)
  (pl:insertf "(")
  (loop for item in indexes 
		and i from 1 do
		(pl:transcode item)
		when (not (= i (length indexes)))
		do (pl:insertf ", "))
  (pl:insertf ")")
  (pl:insertf ".(")
  (pl:transcode field)
  (pl:insertf ")"))


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

(defun-match pl:transcode ((list 'flat-cond))
  "An empty flat cond does nothing.")

(defun-match pl:transcode ((list-rest 'flat-cond bodies))
  (let ((n  (length bodies))
		(start (point)))
	(loop 
	 for body in bodies 
	 and i from 1 do
	 (match body 
			((list-rest condition body)
			 (pl:insertf (if (= i 1) "if " "elseif "))
			 (pl:transcode condition)
			 (pl:insertf "\n")
			 (pl:transcode-sequence body))))
	(pl:insertf "end\n")
	(pl:indent-region start (point))))

(defun-match pl:transcode ((list 'if condition true false))
  "If is transcoded to a functional if."
  (pl:insertf "fif(")
  (pl:transcode condition)
  (pl:insertf ", @()")
  (pl:transcode true)
  (pl:insertf ", @()")
  (pl:transcode false)
  (pl:insertf ")"))

(defun-match pl:transcode ((list 'if condition true))
  "If is transcoded to a functional if."
  (pl:insertf "fif(")
  (pl:transcode condition)
  (pl:insertf ", @()")
  (pl:transcode true)
  (pl:insertf ", @()")
  (pl:transcode nil)
  (pl:insertf ")"))

(defun-match pl:transcode ((list 'try (list-rest code) (list-rest catch-clause)))
  "Transcode a try/catch block."
  (let ((start (point)))
	(pl:insertf "try\n")
	(pl:transcode-sequence code)
	(pl:insertf "catch\n")
	(pl:transcode-sequence catch-clause)
	(pl:insertf "end")))

(defun-match pl:transcode ((list-rest 'or clauses))
  (let ((n (length clauses)))
	(pl:insertf "orFunction(")
	(loop for c in clauses 
		  and i from 1 do 
		  (pl:insertf "@()")
		  (pl:transcode c)
		  when (not (= i n)) do
		  (pl:insertf ", "))
	(pl:insertf ")")))

(defun-match pl:transcode ((list-rest 'and clauses))
  (let ((n (length clauses)))
	(pl:insertf "andFunction(")
	(loop for c in clauses 
		  and i from 1 do 
		  (pl:insertf "@()")
		  (pl:transcode c)
		  when (not (= i n)) do
		  (pl:insertf ", "))
	(pl:insertf ")")))

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
	(pl:indent-region start (point))))

(defun-match pl:transcode ((list-rest 'while expr body))
  (let ((start (point)))
	(pl:insertf "while ")
	(pl:transcode expr)
	(pl:insertf "\n")
	(pl:transcode-sequence body)
	(pl:insertf "end\n")))

(defun-match pl:transcode ((list-rest 'for (list (p #'symbolp index) 
												 (p #'symbolp value)) 
									  expr body))
  "Expand a for loop with index expression."
  (let ((start (point))
		(expr-value (gensym "for-loop-value-")))
	(pl:transcode `(setq ,expr-value ,expr))
	(pl:insertf ";\n")
	(pl:insertf "for ")
	(pl:transcode index)
	(pl:insertf " = ")
	(pl:transcode `(: 1 (length ,expr-value)))
	(pl:insertf "\n")
	(pl:transcode `(setq ,value (,expr-value ,index)))
	(pl:insertf ";\n")
	(pl:transcode-sequence body)
	(pl:insertf "end\n")
	(pl:insertf "clear '%s';\n" (pl:mangle expr-value))
	(pl:indent-region start (point))))

(defun-match pl:transcode ((list-rest 'forcell 
									  (p #'symbolp v) 
									  expr 
									  body))
  (pl:transcode `(for ,v (flat-across ,expr)
					  (setq ,v ({} ,v 1))
					  ,@body)
				))

(defun-match pl:transcode ((list-rest 'forcell 
									  (list (p #'symbolp i)
											(p #'symbolp v)) 
									  expr 
									  body))
  (pl:transcode `(for (,i ,v) ,expr
					  (setq ,v ({} ,v 1))
					  ,@body)))

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
	(pl:indent-region start (point))))

(defun-match pl:transcode ((list 'function (p #'symbolp s)))
  "Encode a function namespace query."
  (pl:insertf "@")
  (pl:transcode s))

(defvar pl:inside-previous-defun nil)
(defun-match pl:transcode ((list-rest 'defun
									  (pl:arglist outargs)
									  (p #'symbolp name) 
									  (pl:arglist inargs) 
									  body))
  "Transcode a function into an m-file."
  
  (let* ((start (point))
		 (output-buffer 
		  (if pl:inside-previous-defun (current-buffer)
			(find-file-noselect (concat (pl:mangle (symbol-name name)) ".m"))))
		 (was-inside pl:inside-previous-defun)
		 (pl:inside-previous-defun t)
		 (pl:disable-indentation 
		  (match pl:disable-indentation 
				 (:always :always)
				 (:outer nil)
				 (nil nil))))
	(with-current-buffer output-buffer
	  (when (not was-inside)
		(delete-region (point-min)
					   (point-max))) 
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
	  (pl:transcode-sequence 
	   (if (stringp (car body)) (cdr body) body))
	  (pl:insertf "end\n")
	  (basic-save-buffer))
	(when (not was-inside)
	  (pl:maybe-kill-buffer output-buffer))))

(defun-match pl:transcode ((list-rest 'defmacro name (p #'listp args) body))
  (eval `(pl:def-pl-macro ,name ,args ,@body)))

(defun-match pl:transcode ((list-rest 'script name body))
  "Encode a body into a script."
  (assert (or (symbolp name)
			  (stringp name))
		  ()
		  "Name must be a string or a symbol.")
  (let* ((outfile-name (if (symbolp name)
						   (concat (pl:mangle (symbol-name name)) ".m")
						 name))
		 (output-buffer (find-file-noselect outfile-name))
		 (pl:disable-indentation (match pl:disable-indentation 
										(:always :always)
										(:outer nil)
										(nil nil))))
	(with-current-buffer output-buffer
	  (delete-region (point-min)
					 (point-max))
	  (pl:transcode-sequence body)
	  (basic-save-buffer))
	(pl:maybe-kill-buffer output-buffer)))

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

(pl:def-pl-macro let* (bindings &body body)
				 (match bindings 
						((list) `(progn ,@body))
						((list-rest (list name expr) rest-bindings)
						 `(with ,name ,expr (let* ,rest-bindings ,@body)))))

(pl:def-pl-macro 
 cond (&rest legs)
 (let* ((branch-bodies 
		 (mapcar (lambda (body)
				   `(lambda () (progn ,@body))) legs))
		(conditions (mapcar (lambda (leg)
							  `(lambda () ,(car leg))) legs))
		(body-formatted 
		 (cons 'cell-array 
			   (mapcar* (lambda (c b)
						  `(cell-array ,c ,b)) conditions 
						  branch-bodies))))
   `(funcall (cond-helper-first-true ,body-formatted))))

(pl:def-pl-macro let (bindings &body body)
				 (match body
						((list real-body)
						 `(funcall (lambda ,(mapcar #'car bindings) ,@body) ,@(mapcar #'cadr bindings)))
						((list-rest real-body) 
						 `(funcall (lambda ,(mapcar #'car bindings) (progn ,@real-body)) ,@(mapcar #'cadr bindings)))))

(defun-match pl:transcode ((list-rest (p #'pl:non-keyword-symbolp the-function) arguments))
  "Handle the-function calls."
  (let ((start (point)))
	(pl:transcode the-function)
	(pl:insertf "(")
	(loop for arg in arguments and
		  i from 1 
		  do 
		  (pl:transcode arg :as-expression)
		  when (< i (length arguments))
		  do
		  (pl:insertf ", ")
		  )
	(pl:insertf ")")
	(pl:indent-region start (point))))

(defun pl:statement-formp (form)
  (match form 
		 ((list-rest 'for _) t)
		 ((list-rest 'forcell _) t)
		 ((list-rest 'flat-cond _) t)
		 ((list-rest 'setq _) t)
		 ((list-rest 'block _) t)
		 ((list-rest 'try _) t)
		 ((list-rest := _) t)
		 ((list-rest 'flat-cond _) t)
		 ((list 'if condition 
				(list-rest 'block _)
				(list-rest 'block _)) t)
		 ((list 'if 
				condition
				(list-rest 'block _)) t)
		 ((and (list-rest (p #'pl:pl-macrop head) _))
		  (pl:statement-formp (apply (pl:pl-macrop head) _)))
		 (_ nil)))

(defun pl:replace-newlines-with-semicolons (s)
  (replace-regexp-in-string 
   (regexp-quote (format "\n")) " ; " s))

(defun-match pl:transcode ((p #'pl:statement-formp the-form) :as-expression)
  "Setq can be transcoded as an expression via the use of evalc and quotation."
  (pl:insertf "evalc(")
  (pl:transcode (pl:replace-newlines-with-semicolons 
				 (pl:transcode-to-string the-form)))
  (pl:insertf ")"))

(defun-match pl:transcode (_ :as-expression)
  "Handle transcoding forms when an expression is required.  Most
forms transcode identically so this is a drop in replacement."
  (recur _))

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
		  "Add as many numbers as you'd like."
		  (setq r 0) 
		  (for x varargin
			   (setq r (+ r ({} x 1)))))

(pl:defun (r) ** (varargin)
		  "Multiply as many numbers as you'd like."
		  (setq r 1) 
		  (for x varargin
			   (setq r (* r ({} x 1)))))

(pl:defun (r) -> (struct varargin)
		  (setq r struct)
		  (for i (: 1 (length varargin))
			   (setq key ({} varargin i))
			   (setq r (struct-access r key))))

(pl:defun (s) extend-struct (s f val)
		  (setq (.. s f) val))

(pl:defun (r) echo-return (q)
		  (fprintf q)
		  (setq r q))

(pl:def-pl-macro dont-do (&rest body)
				 ())

(pl:def-pl-macro let-while (symbol expression &body body)
				 `(block 
					  (setq ,symbol ,expression)
					(while ,symbol ,@body 
						   (setq ,symbol ,expression))))

(pl:def-pl-macro flat-if (pred true &optional false)
				 `(if ,pred (block ,@true) (block ,@false)))

(pl:def-pl-macro empty-matrices (&rest names)
				 `(:= ,@(loop 
						 for name in names append 
						 `(,name nil))))

(pl:defun (r) flat-across (a)
		  "Return a flat version of a, extending column-ward."
		  (setq r (transpose (a :))))

(pl:defun (r) flat-down (a)
		  "Return a flat version of a, extending row-ward."
		  (setq r (a :)))


(defmacro* pl:pl (&body body)
  "Transcode BODY to matlab."
  `(pl:transcode '(block ,@body)))

(pl:def-pl-macro 
 holding (&rest body)
 `(block 
	  (hold :on) ,@body
	  (hold :off)))

(pl:def-pl-macro 
 capture (&rest args)
 (let ((names (gensym "names"))
	   (name (gensym "name"))
	   (struct-name (gensym "struct-name-")))
   (match args
		  ((list :all)
		   `(progn 
			  (setq ,struct-name (struct))
			  (forcell ,name (who)
					   (setq (.. ,struct-name ,name)
							 (eval [ ,name ";"])))
			  ,struct-name))
		  ((list-rest elements)
		   `(progn 
			  (setq ,struct-name (struct))
			  (forcell ,name (cell-array ,@elements)
					   (setq (.. ,struct-name ,name)
							 (eval [ ,name ";"])))
			  ,struct-name)))))

(pl:def-pl-macro flat-when (condition &rest body)
				 `(flat-if ,condition (,@body)))


(provide 'parenlab)

