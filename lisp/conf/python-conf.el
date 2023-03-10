;;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'general)

(autoload 'ext/treesit-python-setup "treesit-extensions")
(autoload 'ext/vterm-with-program "vterm-extensions")

(defun js/vterm-ipython ()
  (interactive)
  (ext/vterm-with-program "ipython"))

(use-package lsp-pyright
  :straight t)

(use-package lsp-mode
  :straight t
  :hook ((python-ts-mode python-mode) . lsp-deferred)
  :config
  (setq
   lsp-pylsp-plugins-jedi-completion-fuzzy t
   lsp-pylsp-plugins-pydocstyle-enabled nil
   lsp-pylsp-plugins-flake8-enabled nil)
  )

;; (with-eval-after-load "python"
;;   (add-hook 'python-mode-hook
;;             #'ext/treesit-python-setup))

(use-package python-black
  :straight t
  :general
  (local-leader-def
    :keymaps '(python-mode-map python-ts-mode-map)
    "=" 'python-black-buffer))          

(use-package python-x
  :straight t
  :after (python)
  :general
  (local-leader-def
    :keymaps '(python-mode-map python-ts-mode-map)
    "e" 'python-shell-send-dwim)
  :config
  (python-x-setup))

(provide 'python-conf)
