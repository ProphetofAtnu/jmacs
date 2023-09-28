;; -*- lexical-binding: t; -*-

(defvar local-package-list nil)

(defmacro use-local! (name relpath &optional files)
  (add-to-list 'local-package-list name)
  `(straight-use-package '(,name :type nil
                                 :local-repo ,(expand-file-name relpath
                                                                user-emacs-directory)
                                 ,@(if files
                                       (list :files files)
                                     nil)
                                 :build t)))

(defun rebuild-local-packages! ()
  (interactive)
  (cl-loop for pkg in local-package-list
           do (straight-rebuild-package (symbol-name pkg))))

(provide 'early/core)
