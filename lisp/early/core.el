;; -*- lexical-binding: t; -*-

;; Utility Packages
(straight-use-package 'dash)
(straight-use-package 'm-buffer)
(straight-use-package 'f)
(straight-use-package 'ts)
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'datetime)
(straight-use-package 'deferred)
(straight-use-package 'async)
(straight-use-package 'emacsql)
(straight-use-package 'emacsql-sqlite-builtin)
(straight-use-package 'zmq)

;; Load paths
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "util" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "util/tools" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "util/elisp-sql-capf" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "platform" user-emacs-directory))


(provide 'early/core)
