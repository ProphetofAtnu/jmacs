;; -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/ext" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/lib" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/conf" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "util" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "util/tools" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "util/elisp-sql-capf" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "platform" user-emacs-directory))

(provide 'early/load-paths)
