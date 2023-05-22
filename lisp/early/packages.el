;; -*- lexical-binding: t; -*-

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
(straight-use-package 'esup)

(straight-use-package 'esup)

(straight-use-package
 `(org-ext
   :type nil
   :local-repo ,(expand-file-name
		 "util"
		 user-emacs-directory)
   :files ("org-ext.el")
   :build t))

(straight-use-package
 `(js-tools
   :type nil
   :local-repo ,(expand-file-name
		 "util/tools"
		 user-emacs-directory)
   :build t))


(provide 'early/packages)
