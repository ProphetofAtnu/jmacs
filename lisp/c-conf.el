;;; -*- lexical-binding: t; -*-

(use-package glsl-mode
    :straight t)

(use-package cmake-mode
    :straight t)

;; (use-package lsp-mode
;;     :straight t
;;     :hook ((objc-mode c-mode c++-mode) . lsp-deferred))

(use-package acm
    :straight t)

(use-package lsp-bridge
    :straight t
    :hook ((objc-mode c-mode c++-mode) . lsp-bridge-mode))



(defconst cmake-export-compile-commands-flag
  "-DCMAKE_EXPORT_COMPILE_COMMANDS=1")

(defvar cmake-emacs-build-dir "emacs-build")
(defvar cmake-emacs-build-tool "make")

(defmacro with-curdir (dirname &rest body)
  `(let ((cur default-directory))
    (cd ,dirname)
    (let ((result
           (progn ,@body)))
      (cd cur)
      result)))

(defun cmake--maybe-link-compile-commands ()
  (let ((dfiles (directory-files (projectile-project-root)))
        (shell-command-dont-erase-buffer t))
    (when (and (member cmake-emacs-build-dir dfiles)
               (not (member "compile_commands.json" dfiles)))
      (with-curdir (projectile-project-root)
        (message
         "%s"
         (shell-command-to-string
          (format
           "ln -s %s/compile_commands.json ."
           cmake-emacs-build-dir)))))))

(defun cmake-emacs-init-or-refresh ()
  (interactive)
  (when (member
         "CMakeLists.txt"
         (directory-files
          (projectile-project-root)))
    (ignore-errors
      (mkdir "emacs-build"))
    (with-curdir (projectile-project-root)
      (message
       "%s"
       (shell-command-to-string
        (format
         "cmake . %s -B %s"
         cmake-export-compile-commands-flag
         cmake-emacs-build-dir))))
    (cmake--maybe-link-compile-commands)))

(defun cmake-emacs-do-build ()
  (interactive)
  (when (member cmake-emacs-build-dir
                (directory-files
                 (projectile-project-root)))
    (with-curdir cmake-emacs-build-dir
      (message "%s"
               (shell-command-to-string
                (format "%s" cmake-emacs-build-tool))))))

(local-leader-def
    :keymaps '(c-mode-map c++-mode-map)
  "R" 'cmake-emacs-init-or-refresh
  "B" 'cmake-emacs-do-build)

(provide 'c-conf)
