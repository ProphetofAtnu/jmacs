;; -*- lexical-binding: t; -*-
;; Builtin Emacs Tree Sitter (BETS) extensions
;; (Not the dynamic module)

(defvar-local bets-buffer-parser nil)

(defun bets--shell-mode-syntax (&optional buffer)
  (buffer-local-value 'sh-shell (or buffer (current-buffer))))

(defvar bets-language-modes-alist
  '((c++-mode . cpp)
    (sh-mode . bets--shell-mode-syntax)))

(defun bets-default-mode-loader (sym)
  (unless (symbolp sym)
    (error "passed a non-symbol"))
  (let ((sstr (symbol-name sym)))
    (intern (string-remove-suffix "-mode" sstr))))

(defun bets-get-mode-language (&optional buffer)
  (let* ((bmode (buffer-local-value
                 'major-mode
                 (or buffer (current-buffer))))
         (override (alist-get bmode bets-language-modes-alist)))
    (or (if (functionp override)
            (funcall override buffer)
          override)
        (bets-default-mode-loader bmode))))

(defun bets-can-enable (&optional buffer)
  (let ((lang (bets-get-mode-language buffer)))
    (if (treesit-language-available-p
         lang)
        lang nil)))

(defun bets-setup-buffer-parser (&optional buffer)
  "Get a parser for the current buffer or 'nil' if there is no valid
parser."
  (or (buffer-local-value
       'bets-buffer-parser
       (or buffer (current-buffer)))
      (when-let ((lang (bets-can-enable buffer)))
	(with-current-buffer (or buffer (current-buffer))
	  (setq-local bets-buffer-parser (treesit-parser-create
					  lang buffer))))))

(defun bets-simple-parser-at-point (&optional pnt)
  (bets-can-enable))


;;;###autoload
(define-minor-mode bets-autostart-mode
  "Automatically load tree sitter for languages that support it"
  :group 'bets
  :global t
  (if bets-autostart-mode
      (add-hook #'prog-mode-hook
		#'bets-setup-buffer-parser)
    (remove-hook #'prog-mode-hook
		 #'bets-setup-buffer-parser)))

(provide 'bets)
