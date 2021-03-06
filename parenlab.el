(require 'shadchen)

(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 2000)

(defvar pl:transplant-scripts-and-functions t)

(defvar pl:disable-indentation nil)
(defvar pl:transcoding-in-elisp nil)

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

(defvar pl:kill-opened-transcode-buffers t)
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

(defvar pl:mangle-cache (make-hash-table :test 'equal))
(defvar pl:unmangle-cache (make-hash-table :test 'equal))
(setq pl:unmangle-cache (make-hash-table :test 'equal))
(setq pl:mangle-cache (make-hash-table :test 'equal))

(defun pl:mangle (s)
  (let* ((s (if (symbolp s) (symbol-name s) s))
		 (m (gethash
			 s pl:mangle-cache)))
	(if m m
	  (progn 
		(let ((m (pl:mangle-raw s)))
		  (setf (gethash s pl:mangle-cache) m)
		  (setf (gethash m pl:unmangle-cache) s)
		  m)))))

(defun pl:attempt-unmangle (input-s)
  (let* ((output-as-symbol (symbolp input-s))
		 (s (if (symbolp input-s) (symbol-name input-s) input-s))
		 (u (gethash s pl:unmangle-cache s)))
	(if output-as-symbol 
		(intern u)
	  u)))

(defun pl:mangle-raw (s)
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
			   s1))
		 (s1 (if (string= "." (substring s1 (- (length s1) 1) (length s1)))
				 (concat (substring s1 0 (- (length s1) 1)) "dot")
			   s1)))
	(replace-regexp-in-string 
	 (rx 
	  (| "|" "~" "+" "-" "*" "%" "$" "&" "^" "!" ":" "/" "\\" "#" "@" "?" "="
		 "<" ">"))
	 (match-lambda
	  ("|" "pipe")
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
		  (if (or (eq '| item)
				  (eq : item)) 
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

(defun pl:join (strings delim)
  "Join the list of strings with DELIM."
  (reduce (lambda (ac it)
			(concat ac delim it))
		  (cdr strings)
		  :initial-value (car strings)))

(defun pl:remove-extension (file)
  (let* ((r (reverse (split-string file (regexp-quote ".")))))
	(if (= 1 (length r)) (car r)
	  (let ((r (cdr r))
			(r (reverse r)))
		(pl:join r ".")))))

(defun pl:transcode-file (file)
  (let* ((buffer (find-file-noselect file))
		 (string-of (with-current-buffer buffer
					  (buffer-substring (point-min) (point-max))))
		 (code (car (read-from-string (concat (format "(script \"%s\" " file) string-of ")")))))
	(message "%S" code)
	(with-current-buffer buffer
	  (pl:transcode code))))

(defvar pl:require-cache (make-hash-table :test 'equal))

(defun pl:find-buffer-by-file-visited (file)
  "Return the buffer visiting FILE, NIL if no such buffer exists."
  (let ((truename (file-truename file)))
	(match-let (((cons hd buffers) (buffer-list)))
			   (cond ((null hd) nil)
					 ((string= truename (buffer-file-name hd)) hd)
					 (:otherwise (recur buffers))))))

(defun pl:file-already-openp (file)
  (pl:find-buffer-by-file-visited file))

(defun pl:md5-file (file)
  "Return the md5 of the file FILE."
  (let ((already-open (pl:file-already-openp file)))
	(prog1 
		(md5 (find-file-noselect file))
	  (if (not already-open)
		  (kill-buffer already-open)))))

(defun pl:file-needs-re-require (file)
  (let ((hash (pl:md5-file file)))
	(not (equal hash (gethash file pl:require-cache)))))

(defun pl:update-require-cache (file)
  (setf (gethash file pl:require-cache) (pl:md5-file file)))

(defun-match pl:transcode ((list-rest 'require files))
  "Files should be a list of parenlab files which are transcoded
  when the require line is encountered."
  (loop for f in files do
		(when (pl:file-needs-re-require f)
		  (pl:transcode `(block (addpath (file-directory ,f))))
		  (pl:transcode-file f)
		  (pl:update-require-cache f))))

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

(defun pl:make-sequence-set-last-to (name sequence)
  (let* ((re (reverse (copy-list sequence)))
		 (last (car re))
		 (new-last `(setq ,name ,last)))
	(reverse (cons new-last (cdr re)))))

(defun-match pl:transcode ((list-rest 'progn forms))
  "Progn is transcoded to a hack function call."
  (cond (pl:inside-previous-defun
		 (let ((ref (gensym "progn-name-"))
			   (retval (gensym "progn-retval-")))
		   (pl:insertf "(")
		   (pl:transcode ref)
		   (pl:insertf ")")
		   (push 
			(list (list retval) ref (list) 
				  (pl:make-sequence-set-last-to retval forms))
			pl:deferred-functions)))
		((not pl:inside-previous-defun)
		 (pl:insertf "progn(")
		 (loop for form in forms do
			   (pl:transcode form :as-expression) 
			   (pl:insertf ", "))
		 (delete-region (point) (- (point) 2))
		 (pl:insertf ")"))))

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
  (cond 
   ((not pl:inside-previous-defun)
	(pl:insertf "@(")
	(loop for arg in args 
		  and i from 0 do
		  (pl:transcode arg)
		  when (< i (- (length args) 1))
		  do (pl:insertf ", "))
	(pl:insertf ")")
	(pl:transcode form))
   (pl:inside-previous-defun 
	(let ((name (gensym "lambda-"))
		  (retval (gensym "retval-")))
	  (pl:transcode `(function ,name))
	  (push (list (list retval) name args 
				  (pl:make-sequence-set-last-to retval (list form)))
			pl:deferred-functions)))))

(defun-match pl:transcode ((list-rest (and (or 'for 'forcell) head) 
									  :initialize name/values rest))
  (pl:transcode `(block (:= ,@name/values)
				   (,head ,@rest))))

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
  "A  for loop over a cell array, without index."
  (pl:transcode `(for ,v (flat-across ,expr)
					  (setq ,v ({} ,v 1))
					  ,@body)
				))

(defun-match pl:transcode ((list-rest 'forcell 
									  (list (p #'symbolp i)
											(p #'symbolp v)) 
									  expr 
									  body))
  "A  for loop over a cell array, with an index.."
  (pl:transcode `(for (,i ,v) (flat-across ,expr)
					  (setq ,v ({} ,v 1))
					  ,@body)))

(defun-match pl:transcode ((list-rest 'forstruct 
									  (p #'symbolp v)
									  expr
									  body))
  "A for loop over struct elements without an index."
  (pl:transcode `(for ,v (flat-across ,expr)
					  ,@body)))

(defun-match pl:transcode ((list-rest 'forstruct 
									  (list (p #'symbolp i)
											(p #'symbolp v))
									  expr
									  body))
  "A for loop over struct elements without an index."
  (pl:transcode `(for (,i ,v) (flat-across ,expr)
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

(defun-match pl:transcode ((list 'function (p #'pl:non-keyword-symbolp s)))
  "Encode a function namespace query."
  (pl:insertf "@")
  (pl:transcode s))

(defun-match pl:transcode ((list 'function (p #'keywordp s)))
  "Encode a curried struct field access."
  (pl:transcode `(struct-access/c ,s)))


(defvar pl:inside-previous-defun nil)

(defun pl:transcode-comma-seperated-list (expressions)
  (let ((n-expressions (length expressions)))
	(loop for e in expressions 
		  and i from 1 do
		  (pl:transcode e)
		  when (not (= i n-expressions)) do
		  (pl:insertf ", "))))
(defvar pl:deferred-functions nil)
(defun pl:transcode-defun-raw (outargs name inargs body)
  (pl:insertf "function [")
  (pl:transcode-comma-seperated-list outargs)
  (pl:insertf "] = ")
  (pl:transcode name)
  (pl:insertf "(")
  (pl:transcode-comma-seperated-list inargs)
  (pl:insertf ")\n")
  (let ((pl:deferred-functions nil)
		(doc-string (if (stringp (car body)) (car body) ""))
		(actual-body (if (stringp (car body)) (cdr body) body)))
	(pl:insertf "%s\n" (pl:fix-comment-string doc-string))
	(pl:transcode-sequence actual-body)
	(loop for d in pl:deferred-functions do
		  (apply #'pl:transcode-defun-raw d)
		  (pl:insertf "\n")))
  (pl:insertf "\nend"))

(defun-match pl:transcode 
  ((list-rest 'defun (pl:arglist outargs)
			  (p #'symbolp name)
			  (pl:arglist inargs)
			  body))
  (unless pl:transcoding-in-elisp 
	(pl:transcode `(function ,name)))
  (cond 
   (pl:inside-previous-defun 
	(push (list outargs name inargs body) pl:deferred-functions))
   ((not pl:inside-previous-defun)
	(let ((output-buffer 
		   (find-file-noselect (concat (pl:mangle (symbol-name name)) ".m")))
		  (pl:inside-previous-defun t))
	  (with-current-buffer output-buffer
		(delete-region (point-min) (point-max))
		(pl:transcode-defun-raw outargs name inargs body)
		(basic-save-buffer)
		)
	  (pl:maybe-kill-buffer output-buffer)))))

(defun-match pl:transcode 
  ((list-rest 'defun (p #'symbolp name) (pl:arglist inargs) body))
  "Transcode a simple function."
  (let ((o (gensym "o")))
	(pl:transcode 
	 `(defun (,o) ,name ,inargs ,@(pl:make-sequence-set-last-to o body)))))

(defun-match pl:transcode ((list-rest 'defmacro name (p #'listp args) body))
  (eval `(pl:def-pl-macro ,name ,args ,@body)))

(defun pl:ends-with-dotmp (s)
  (and (> (length s) 1)
	   (string= (substring s -2) ".m")))


(defun pl:maybe-add-dotm (s)
  (if (pl:ends-with-dotmp s) (format "%s" s)
	(concat (pl:remove-extension (format "%s" s)) ".m")))

(defun-match pl:transcode ((list-rest 'script name body))
  "Encode a body into a script."
  (assert (or (symbolp name)
			  (stringp name))
		  ()
		  "Name must be a string or a symbol.")
  (let* ((outfile-name (if (symbolp name)
						   (concat (pl:mangle (symbol-name name)) ".m")
						 (pl:maybe-add-dotm name)))
		 (output-buffer (find-file-noselect outfile-name))
		 (pl:disable-indentation (match pl:disable-indentation 
										(:always :always)
										(:outer nil)
										(nil nil))))
	;; (unless pl:transcoding-in-elisp 
	;;   (pl:insertf "%% transcoded script : %s" name))
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

(defun-match pl:transcode ((list-rest 'matrix v))
  "Handle vector."
  (let ((start (point)))
	(pl:insertf "[ ")
	(loop for item in v do
		  (if (or (eq '| item)
				  (eq : item))
			  (pl:insertf ";\n")
			(progn (pl:transcode item)
				   (pl:insertf " "))))
	(pl:insertf "]")
	(pl:indent-region start (point))))

(defun-match pl:transcode ((list-rest 'matrix{} v))
  "Handle vector."
  (let ((start (point)))
	(pl:insertf "{ ")
	(loop for item in v do
		  (if (or (eq '| item)
				  (eq : item)) 
			  (pl:insertf ";\n")
			(progn (pl:transcode item)
				   (pl:insertf " "))))
	(pl:insertf "}")
	(pl:indent-region start (point))))

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
				   `(lambda () (progn ,@(cdr body)))) legs))
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
  "Handle the function calls."
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

(defun-match pl:transcode ((list-rest (p #'keywordp the-keyword) (list argument)))
  "Handle the keyword function calls.  These calls assume their
  argument is a struct and return the corresponding field."
  (let ((struct-name (gensym "struct-name-")))
	(pl:transcode `(let ((,struct-name ,argument))
					 (.. ,struct-name ,the-keyword)))))

(defun-match pl:transcode ((list-rest (p #'keywordp the-keyword) (list (list-rest indexes) argument)))
  "Handle the keyword function calls.  These calls assume their
  argument is a struct and return the corresponding field.  This
  version indexes the struct before getting the field."
  (let ((struct-name (gensym "struct-name-")))
	(pl:transcode `(let ((,struct-name ,argument))
					 (.. ,struct-name ,indexes ,the-keyword)))))

(defun-match pl:transcode ((list-rest (p #'stringp s) arguments))
  "Raise an error if one attempts to apply a string."
  (error "Can't apply a string to arguments."))

(defun-match pl:transcode ((list-rest function-expression args))
  "If the function is an arbitrary expression, find its value and
funcall that."
  (let ((the-function (gensym "the-function-")))
	(pl:transcode `(let ((,the-function ,function-expression))
					 (funcall ,the-function ,@args)))))

(defun pl:statement-formp (form)
  (match form 
		 ((list-rest 'for _) t)
		 ((list-rest 'forcell _) t)
		 ((list-rest 'flat-cond _) t)
		 ((list-rest 'setq _) t)
		 ((list-rest 'block _) t)
		 ((list-rest 'try _) t)
		 ((list-rest := _) t)
										;((list-rest 'defun) t)
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
(defun-match pl:maybe-empty-list-of-symbols (_) nil)

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
  `(let ((pl:transcoding-in-elisp t))
	 (pl:transcode '(defun ,oargs ,name ,iargs ,@body))))

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

(pl:defun (g) /and (varargin)
		  "Return a function G which is the logical and of the results of passing args to all of the functions in varargin."
		  (:= functions varargin)
		  (defun (b) /and-inner (varargin)
			(:= b (funcall (first functions varargin)))
			(forcell fun ({} functions (: 2 end)) 
					 (:= b (and b (funcall fun varargin)))
					 (flat-when (not b)
								return))
			))

(pl:defun (files) directory-files (the-dir post-filter)
		  "Return FILES in THE-DIR, as strings, excluding
directories and anything which fails post-filter, which
is by default always true."
		  (flat-when (not (strcmp (the-dir end) "/"))
					 (:= the-dir [the-dir "/"]))
		  (flat-when (not (exist 'post-filter))
					 (:= post-filter (lambda (x) 1)))
		  (:= initial-files (dir the-dir))
		  (:= files (cell-array))
		  (forstruct s initial-files
					 (:= name [the-dir s.name])
					 (flat-when (and (not s.isdir)
									 (funcall post-filter name))
								(:= ({} files (+ end 1)) name))))

(pl:defun (files) directory-directories (the-dir post-filter)
		  "Return FILES in THE-DIR, as strings, excluding
directories and anything which fails post-filter, which
is by default always true."
		  (flat-when (not (strcmp (the-dir end) "/"))
					 (:= the-dir [the-dir "/"]))
		  (flat-when (not (exist 'post-filter))
					 (:= post-filter (lambda (x) 1)))
		  (:= initial-files (dir the-dir))
		  (:= files (cell-array))
		  (forstruct s initial-files
					 (:= name [the-dir s.name])
					 (flat-when (and s.isdir
									 (funcall post-filter name)
									 (not (strcmp ".." s.name))
									 (not (strcmp "." s.name)))
								(:= ({} files (+ end 1)) name))))

(pl:defun (files) directory-contents (the-dir post-filter)
		  "Return DIRECTORIES in THE-DIR, as strings, excluding
directories and anything which fails post-filter, which is by
default always true."
		  (flat-when (not (strcmp (the-dir end) "/"))
					 (:= the-dir [the-dir "/"]))
		  (flat-when (not (exist 'post-filter))
					 (:= post-filter (lambda (x) 1)))
		  (:= initial-files (dir the-dir))
		  (:= files (cell-array))
		  (forstruct s initial-files
					 (:= name [the-dir s.name])
					 (:= ({} files (+ end 1)) name)))

(pl:defun (files) tree-files (root post-filter)
		  "Return the file-contents of the directory 
ROOT. subject to the filter POST-FILTER."
		  (flat-when (not (strcmp (root end) "/"))
					 (:= the-dir [root "/"]))
		  (flat-when (not (exist 'post-filter))
					 (:= post-filter (lambda (x) 1)))
		  (:= files (directory-files root post-filter))
		  (:= sub-directories (directory-directories root post-filter))
		  (forcell (i sub-root) sub-directories
				   (:= files 
					   [files (tree-files sub-root post-filter)])))

(pl:defun (name) file-name (whole-file)
		  "Return the name part of a file."
		  (setq ii (find (== whole-file "/")))
		  (flat-if (isempty ii)
				   ((setq name whole-file))
				   ((setq name (whole-file (: (+ 1 (ii end)) end))))))

(pl:defun (the-dir) file-directory (whole-file)
		  "Return the directory part of a file."
		  (setq ii (find (== whole-file "/")))
		  (flat-if (isempty ii)
				   ((setq the-dir whole-file))
				   ((setq the-dir (whole-file (: 1 (- (ii end) 1)))))))

(pl:defun (r) cell-equal (c1 c2)
		  (flat-when (not (all (== (size c2) (size c1))))
					 (:= r 0)
					 return)
		  (:= c1 (flat-across c1)
			  c2 (flat-across c2))
		  (forcell (i c1-el) c1
				   (:= c2-el ({} c2 i))
				   (flat-when (not (equal c1-el c2-el))
							  (:= r 0)
							  return))
		  (:= r 1))

(pl:defun (r) count-of (o)
		  (setq r (length (flat-across o))))

(pl:defun (r) struct-equal (s1 s2)
		  (setq fields1 (sort (fieldnames s1))
				fields2 (sort (fieldnames s2)))
		  (flat-cond 
		   ((and (all (== (size s1) (size s2)))
				 (== (length (fieldnames s1))
					 (length (fieldnames s2)))
				 (all (strcmp fields1 fields2)))
			(flat-cond 
			 ((== (count-of s1) 1)
			  (forcell (i f) fields1
					   (flat-when (not (equal (.. s1 f)
											  (.. s2 f)))
								  (:= r 0)
								  return))
			  (:= r 1))
			 (:otherwise 
			  (for (i ss1) s1
				   (:= ss2 (s2 i))
				   (flat-when (not (equal ss1 ss2))
							  (:= r 0)
							  return))
			  (:= r 1))))
		   (:otherwise (:= r 0))))

(pl:defun (r) isclass (class-name object)
		  (:= r (strcmp (class object) class-name)))

(pl:defun (r) equal (a b)
		  "Generalized, structural equality."
		  (flat-cond 
		   ((strcmp (class a) (class b))
			(flat-cond 
			 ((isclass "cell" a)
			  (setq r (cell-equal a b)))
			 ((isclass "struct" a)
			  (setq r (struct-equal a b)))
			 ((isclass "char" a)
			  (setq r (strcmp a b)))
			 ((and (isnumeric a)
				   (isnumeric b))
			  (setq r (all (== a b))))))
		   ((and (isnumeric a)
				 (isnumeric b))
			(setq r (all (== a b))))
		   (:otherwise 
			(setq r 0))))

(pl:def-pl-macro 
 flat-case 
 (expression &rest legs)
 (let ((value (gensym "case-value-")))
   `(block 
		(:= ,value ,expression)
	  (flat-cond 
	   ,@(loop 
		  for leg in legs collect
		  `((equal ,value ,(car leg))
			,@(cdr leg)))))))

(pl:defun (b) equal-one-of (value values)
		  (:= b 0)
		  (forcell (i value*) values
				   (flat-when (equal value value*)
							  (:= b 1)
							  return)))

(pl:def-pl-macro 
 case 
 (expression &rest legs)
 (let ((value (gensym "case-value-")))
   `(with  
	 ,value ,expression
	 (cond 
	  ,@(loop 
		 for leg in legs collect
		 (match leg 
				((list-rest (or :otherwise 'otherwise 'else :else) body)
				 `(1 ,@body))
				((list-rest (list-rest values) body)
				 `((equal-one-of ,value (cell-array ,@values)) ,@body))
				((list-rest (p #'symbolp value) body)
				 `((equal ,value ,expression) ,@body))))))))

(pl:defun (b) struct-access/c (field)
		  "Return a function which returns the FIELD of the struct."
		  (setq b 
				(lambda (the-struct)
				  [(.. the-struct field)])))

(pl:defun (ignore) delete* (file varargin)
		  "Just like DELETE* except deletes multiple files and returns a value."
		  (setq ignore 0)
		  (delete file)
		  (forcell (i v) varargin
				   (delete v)))

(pl:defun (s) numeric->rstring (o)
  (flat-cond 
   ((== 1 (length o))
	(setq s (sprintf "%d" o)))
   ((== 2 (length (size o)))
	(for :initialize (s "(matrix")
		 i (: 1 (size o 1))
		 (for j (: 1 (size o 2))
	 		  (setq s [ s " " (numeric->rstring (o i j))]))
		 (flat-when (not (== i (size o 1)))
					(setq s [ s " :"])))
	(setq s [ s ")"]))
   (:otherwise 
	(setq s ["(reshape " (numeric->rstring (flat-down o)) " " (numeric->rstring (size o)) ")"]))))

(pl:defun (s) string->rstring (ms)
		  (for :initialize (s "\"") 
			   c ms
			   (flat-if (strcmp c "\"")
						((setq s [ s "\\\""]))
						((setq s [ s c]))))
		  (setq s [s "\""]))

(pl:defun (s) cell->rstring (c)
		  (flat-cond 
		   ((== 1 (length c))
			(setq s (sprintf "(cell-array %s)" (object->pl-string (first c)))))
		   ((== 2 (length (size c)))
			(for :initialize (s "(matrix{}")
				 i (: 1 (size c 1))
				 (for j (: 1 (size c 2))
					  (setq s [ s " " (object->pl-string ({} c i j))]))
				 (flat-when (not (== i (size c 1)))
							(setq s [ s " :"])))
			(setq s [ s ")"]))
		   (:otherwise 
			(setq s ["(reshape " (cell->rstring (flat-down c)) " " (numeric->rstring (size c)) ")"]))))


(pl:defun (s) object->pl-string (o)
		  (setq s 
				(case (class o)
				  ((:double :logical :integer)
				   (numeric->rstring o))
				  ((:char)
				   (string->rstring o)))))



(provide 'parenlab)

