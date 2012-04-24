(require 'parenlab)
(require 'shadchen)

(defvar pla:matlab-buffer "*evalshell*")

(defun setup-result-catcher ())

(defun parenlab-do (p)
  (interactive "P")
  
  (let* ((code-string 
		  (if p
			  (read-from-minibuffer "pl: ")
			(concat "(" (buffer-substring (point-min) (point-max)) 
					")")))
		 (code (append (list 'script (intern (replace-string-in-string ".parenlab" "" (buffer-name)))) (car (read-from-string code-string)))))
	
	(pl:transcode code)
	(comint-send-strings (get-buffer pla:matlab-buffer)
						 (pl:mangle (replace-string-in-string ".parenlab" "" (buffer-name))))))

(defun parenlab-do-last-sexp ()
  (interactive "")
  (save-excursion 
	(let* ((code (get-last-sexp))
		   (code `(emessage (evalc ',code))))
	  ;; Note that the above code uses my personal matlab function
	  ;; emessage which sends a message to emacs from matlab, it is
	  ;; not included with this library.
	  (let ((pl:disable-indentation :outer))
		(comint-send-strings (get-buffer pla:matlab-buffer)
							 (pl:replace-newlines-with-semicolons
							  (pl:transcode-to-string code)))))))

(defun parenlab-do-region (s e)
  (interactive "r")
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

