;; -*- lexical-binding: t; -*-
;; Builtin Emacs Tree Sitter
;; (Not the dynamic module)

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

(defun bets-get-buffer-parser (&optional buffer)
  "Get a parser for the current buffer or 'nil' if there is no valid
parser."
  (when-let ((lang (bets-can-enable buffer)))
    (treesit-parser-create
     lang buffer)))

(provide 'bets)
