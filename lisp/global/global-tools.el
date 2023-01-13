;; -*- lexical-binding: t; -*-

(use-package winner
  :general
  ('winner-mode-map
   "C-M-<" 'winner-undo
   "C-M->" 'winner-redo
   )
  :hook (emacs-startup . winner-mode))

(use-package markdown-mode
  :mode "\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'")

(use-package magit
  :straight t
  :commands (magit)
  :general
  (:keymaps 'prefix-project-map
            "m" 'magit))

(use-package helpful
  :straight t
  :init
  (defvar read-symbol-positions-list nil)
  :commands (helpful-symbol
             helpful-key))

(use-package projectile
  :straight t
  :hook (emacs-startup . projectile-mode)
  :general
  (:keymaps 'prefix-file-map
            "p" 'projectile-find-file)
  (:keymaps 'prefix-project-map
            "s" 'projectile-switch-project
            "a" 'projectile-add-known-project
            "R" 'projectile-remove-current-project-from-known-projects
            "t" 'projectile-regenerate-tags
            "r" 'projectile-ripgrep
            "b" 'consult-projectile-switch-to-buffer
            "!" 'projectile-cleanup-known-projects
            "&" 'projectile-run-async-shell-command-in-root
            "w" 'projectile-run-term
            "o" 'projectile-find-other-file
            "x" 'projectile-run-project
            "X" 'projectile-compile-project
            "f" 'projectile-find-file-dwim
            "c" 'projectile-commander
            "C" 'projectile-kill-buffers))

(use-package ace-window
  :straight t
  :commands (ace-window
             ace-swap-window)
  :config
  (setq aw-keys
        (cl-loop for k across "asdfjkl;"
                 collect k)))

(use-package switch-window
  :straight t
  :config
  (setq switch-window-shortcut-style 'qwerty
        switch-window-multiple-frames t))

(use-package zoxide
  :straight t
  :general
  (prefix-file-map
   "z" 'zoxide-find-file
   "t" 'zoxide-travel
   "c" 'zoxide-cd))


(provide 'global/global-tools)
