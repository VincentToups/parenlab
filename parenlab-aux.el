(require 'parenlab)
(require 'shadchen)
(provide 'parenlab-aux)

(defvar pla:matlab-buffer "*evalshell*")
(defvar pla:temp-script-dir "/tmp/parenlab-temp/")

(when (not (directoryp pla:temp-script-dir))
  (make-directory pla:temp-script-dir))

(defun setup-result-catcher ())

(defun pla:join-strings (strings delim)
  (reduce 
   (lambda (ac it)
	 (concat ac delim it ))
   (cdr strings)
   :initial-value (car strings)))

(defun pla:length=1 (o)
  (and (listp o)
	   (= 1 (length o))))

(defun pla:no-dots (o)
  (and (stringp o)
	   (pla:length=1 (split-string o (regexp-quote ".")))))

(defun pla:remove-file-extension (file)
  (let ((parts (split-string file (regexp-quote "."))))
	(if (pla:length=1 parts) (car parts)
	  (pla:join-strings (reverse (cdr (reverse parts))) "."))))

(defun parenlab-do (p)
  (interactive "P")
  (pla:do-macros)
  (let* ((code-string 
		  (if p
			  (read-from-minibuffer "pl: ")
			(concat "(" (buffer-substring (point-min) (point-max)) 
					")")))
		 (code (append (list 'script (intern (pla:remove-file-extension (buffer-name)))) (car (read-from-string code-string)))))
	
	(pl:transcode code)
	(comint-send-strings (get-buffer pla:matlab-buffer)
						 (pl:mangle (replace-string-in-string ".parenlab" "" (buffer-name))))))


(pl:def-pl-macro with-error-to-emacs (&rest body)
				 (let ((error-name (gensym "error-name-")))
				   `(try ,body 
						 ((:= ,error-name (lasterror))
						  (emessage (.. ,error-name :message))
						  (rethrow ,error-name)
						  (clear ',error-name)))))

(defun-match- pla:generate-last-sexp-code ((list-rest (or 'setq :=) pairs) filename)
  "Print the value of the set variable."
  (let ((variable (cadr (reverse pairs))))
	`(script ,filename (setq ,@pairs)
			 (emessage ,variable)
			 )))

(defun-match pla:generate-last-sexp-code ((p #'pl:statement-formp code) filename)
  `(script ,filename ,code (emessage "Done!")))
(defun-match pla:generate-last-sexp-code (code filename)
  (let ((val (gensym "val")))
	`(script ,filename
			 (setq ,val ,code)
			 (fprintf "\\n")
			 (disp ,val)
			 (fprintf "\\n")
			 (emessage (evalc '(disp ,val)))
			 (clear ',val))))

(defun parenlab-do-last-sexp ()
  (interactive "")
  (pla:do-macros)
  (save-excursion 
	(let* ((code (get-last-sexp))
		   (file-name-only (concat (symbol-name (gensym "parenlab"))))
		   (filename 
			(concat pla:temp-script-dir 
					file-name-only
					".m"))
		   (code (pla:generate-last-sexp-code code filename))
		   (run-code (pl:transcode-to-string 
					  `(with-error-to-emacs ,(intern file-name-only)))))
	  ;; Note that the above code uses my personal matlab function
	  ;; emessage which sends a message to emacs from matlab, it is
	  ;; not included with this library.
	  (comint-send-strings (get-buffer pla:matlab-buffer)
						   (format "addpath('%s');" pla:temp-script-dir))
	  (pl:transcode code)
	  (let ((pl:disable-indentation :outer))
		(comint-send-strings (get-buffer pla:matlab-buffer)
							 run-code)))))


(defun parenlab-do-region (s e)
  (interactive "r")
  (pla:do-macros)
  (let ((string (concat "(block "
						(buffer-substring s e)
						")")))
	(comint-send-strings (get-buffer pla:matlab-buffer)
						 (pl:transcode-to-string 
						  (car (read-from-string string))))))


(add-to-list 'auto-mode-alist (cons "\\.parenlab" 
									(lambda ()
									  (lisp-mode)
									  (local-set-key (kbd "C-c C-c") #'parenlab-do)
									  (local-set-key (kbd "C-x C-e") #'parenlab-do-last-sexp))))

(defun pval (sexpr)
  (interactive "x")
  (mval (pl:transcode-to-string sexpr)))

(defvar pla:already-doing-macros nil)
(defun pla:do-macros ()
  (when (and (file-exists-p "macros.parenlab") (not pla:already-doing-macros))
	(with-current-buffer (find-file-noselect "macros.parenlab")
	  (let ((pla:already-doing-macros t))
		(parenlab-do-region (point-min) (point-max))))))

