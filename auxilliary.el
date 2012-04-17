(require 'parenlab)
(require 'shadchen)

(defvar pla:matlab-buffer "*evalshell*")

(defun setup-result-catcher ())

(defun parenlab-do (p)
  (interactive "P")
  (message "%S" p)
  (let* ((code-string 
		  (if p
			  (read-from-minibuffer "pl: ")
			(concat "(" (buffer-substring (point-min) (point-max)) 
					")")))
		 (code (append (list 'script (intern (replace-string-in-string ".parenlab" "" (buffer-name)))) (car (read-from-string code-string)))))
	(message "%S" code)
	(pl:transcode code)
	(comint-send-strings (get-buffer pla:matlab-buffer)
						 (pl:mangle (replace-string-in-string ".parenlab" "" (buffer-name))))))

(defun parenlab-do-last-sexp ()
  (interactive "")
  (save-excursion 
	(let ((code (get-last-sexp)))
	  (comint-send-strings (get-buffer pla:matlab-buffer)
						   (pl:transcode-to-string code)))))

(add-to-list 'auto-mode-alist (cons "\\.parenlab" 
									(lambda ()
									  (lisp-mode)
									  (local-set-key (kbd "C-c C-c") #'parenlab-do)
									  (local-set-key (kbd "C-x C-e") #'parenlab-do-last-sexp))))

