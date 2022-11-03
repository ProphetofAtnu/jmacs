;; -*- lexical-binding: t; -*-

(require 'elixir-mode)
(require 'term)

(defvar-local iex-terminal-project nil)

(defun iex-find-project-dir ()
  (locate-dominating-file default-directory "mix.exs"))

(defun iex-on-start-mix-term ()
  (setq iex-terminal-project default-directory))

(defun iex-run-term ()
  (interactive)
  (pop-to-buffer
   (with-current-buffer (make-term "iex" "iex")
     (term-char-mode)
     (current-buffer))))

(defun iex-run-mix-term ()
  (interactive)
  (let  ((dir (iex-find-project-dir)))
    (if dir 
	(pop-to-buffer
	 (let ((default-directory dir))
	   (with-current-buffer (make-term
				 (format  "iex %s" dir) "iex" nil "-S" "mix")
	     (term-char-mode)
	     (iex-on-start-mix-term)
	     (current-buffer))))
	(iex-run-term))))

(defun iex-project-term-name ()
  (let ((pd (iex-find-project-dir)))
    (when pd (format "*iex %s*" pd))))

(defmacro iex-with-cleared-line (proc &rest body)
  `(progn (process-send-string ,proc "\x15")
	  ,@body
	  (process-send-string ,proc "\x19")))

(defun iex-recompile ()
  (interactive)
  (when-let ((base (iex-find-project-dir)))
	    (with-current-buffer (get-buffer (format "*iex %s*" base))
	      (iex-with-cleared-line (get-buffer-process (current-buffer))
				     (process-send-string (get-buffer-process (current-buffer))
							  "recompile()\n")))))
(provide 'iex)
