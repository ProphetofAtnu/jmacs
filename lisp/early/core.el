;; -*- lexical-binding: t; -*-

(defmacro use-local! (name relpath &optional files)
  `(straight-use-package '(,name :type nil
				 :local-repo ,(expand-file-name relpath
								user-emacs-directory)
				 ,@(if files
				       (list :files files)
				     nil)
				 :build t)))

(provide 'early/core)
