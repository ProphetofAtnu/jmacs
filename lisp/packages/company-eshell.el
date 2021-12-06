;;; -*- lexical-binding: t; -*-

(require 'company)
(require 'company-elisp)

(defun company-eshell--prefix ()
  (let ((prefix (company-grab-symbol)))
    (if prefix
        (when (if (company-in-string-or-comment)
                  (= (char-before (- (point) (length prefix))) ?`)
                (company-eshell--should-complete))
          prefix)
      'stop)))

(defun company-eshell--should-complete ()
  (let ((start (point))
        (depth (car (syntax-ppss))))
    (not
     (when (> depth 0)
       (save-excursion
         (up-list (- depth))
         (when (looking-at company-elisp-defuns-regexp)
           (forward-char)
           (forward-sexp 1)
           (unless (= (point) start)
             (condition-case nil
                 (let ((args-end (scan-sexps (point) 2)))
                   (or (null args-end)
                       (> args-end start)))
               (scan-error
                t)))))))))

(defun company-eshell (command &optional arg &rest ignored)
  "`company-mode' completion backend for Emacs Lisp."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-eshell))
    (prefix (and (derived-mode-p 'eshell-mode)
                 (company-eshell--prefix)))
    (candidates (company-elisp-candidates arg))
    (sorted company-elisp-show-locals-first)
    (meta (company-elisp--doc arg))
    (doc-buffer (let ((symbol (intern arg)))
                  (save-window-excursion
                    (ignore-errors
                      (cond
                        ((fboundp symbol) (describe-function symbol))
                        ((boundp symbol) (describe-variable symbol))
                        ((featurep symbol) (describe-package symbol))
                        ((facep symbol) (describe-face symbol))
                        (t (signal 'user-error nil)))
                      (help-buffer)))))
    (location (let ((sym (intern arg)))
                (cond
                  ((fboundp sym) (find-definition-noselect sym nil))
                  ((boundp sym) (find-definition-noselect sym 'defvar))
                  ((featurep sym) (cons (find-file-noselect (find-library-name
                                                             (symbol-name sym)))
                                        0))
                  ((facep sym) (find-definition-noselect sym 'defface)))))))

(provide 'company-eshell)
