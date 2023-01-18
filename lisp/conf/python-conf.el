;;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'general)

(autoload 'ext/treesit-python-setup "treesit-extensions")

;; (use-package elpy
;;     :straight t
;;     :general
;;     (local-leader-def
;;         :keymaps 'elpy-mode-map
;;       "d" 'elpy-shell-send-statement)
;;     :init
;;     (elpy-enable)
;;     (delq 'elpy-module-company elpy-modules)
;;     (delq 'elpy-module-flymake elpy-modules)
;;     (add-to-list 'popup-buffer-identifiers "\\*Python Doc\\*")
;;     :config
;;     ;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;;     ;;              "jupyter")
;;     ;; (setq python-shell-interpreter "jupyter"
;;     ;;   python-shell-interpreter-args "console --simple-prompt"
;;     ;;   python-shell-prompt-detect-failure-warning nil)
;;     (add-hook 'elpy-mode-hook
;;               #'(lambda ()
;;                   (setq-local evil-lookup-func #'elpy-doc)
;;                   (add-hook 'completion-at-point-functions
;;                             (cape-company-to-capf 'elpy-company-backend) 0 t))))

;; (use-package anaconda-mode
;;     :straight t
;;     :hook (python-mode . anaconda-mode))

;; (use-package lsp-pyright
;;   :straight t)

;; (use-package lsp-mode
;;   :straight t
;;   :hook (python-mode . lsp-deferred)
;;   :config
;;   (setq
;;    lsp-pylsp-plugins-jedi-completion-fuzzy t
;;    lsp-pylsp-plugins-pydocstyle-enabled nil
;;    lsp-pylsp-plugins-flake8-enabled nil)
;;   )

;; (use-package lsp-bridge
;;   :straight t
;;   :hook (python-mode . (lambda ()
;;                          (if buffer-file-name 
;;                              (lsp-bridge-mode))))
;;   )

(with-eval-after-load "python"
  (add-hook 'python-mode-hook
	    #'ext/treesit-python-setup))

(use-package eglot
  :straight t
  :hook (python-mode . eglot-ensure))

(use-package python-black
  :straight t
  :general
  (local-leader-def
    :keymaps '(python-mode-map)
    "=" 'python-black-buffer))          

(use-package python-x
  :straight t
  :after (python)
  :general
  (local-leader-def
    :keymaps '(python-mode-map)
    "e" 'python-shell-send-dwim)
  :config
  (python-x-setup))

(provide 'python-conf)
