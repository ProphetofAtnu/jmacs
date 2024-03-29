;;; -*- lexical-binding: t; -*-

(define-prefix-map dap)

(use-package lsp-mode
  :straight t
  :defer t
  :hook (lsp-mode . lsp-lens-mode)
  :general
  :config
  (local-leader-def
    :definer 'minor-mode
    :keymaps '(lsp-mode)
    "." 'lsp-execute-code-action
    "*" 'lsp-restart-workspace
    "`" 'lsp-treemacs-symbols
    "/" 'lsp-avy-lens
    "d" (mount-prefix-map dap "Dap")
    "=" 'lsp-format-buffer)
  (general-defs
    :keymaps 'prefix-utility-map
    "z" 'lsp)
  ;; Uncomment following section if you would like to tune lsp-mode performance according to
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (defun override-lsp-defaults ()
    (setq-local completion-category-defaults nil))
  ;; (with-eval-after-load 'lsp-mode
  ;;   (eval '(setf (lsp-session-folders
  ;;                 (lsp-session))
  ;;                (cl-remove-if-not #'file-exists-p
  ;;                                  (lsp-session-folders (lsp-session))))))
  (add-hook 'lsp-completion-mode-hook #'override-lsp-defaults)
  (add-hook 'lsp-mode-hook #'company-mode)
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil)
  (setq lsp-file-watch-threshold 5000)
  (setq lsp-completion-provider :none)
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-prefer-flymake t)
  (with-eval-after-load "flycheck"
    (setq flycheck-display-errors-function nil))
  (setq lsp-ui-sideline-enable nil))

(use-package lsp-ui
  :straight t
  :after (lsp-mode)
  :hook (lsp-mode . (lambda ()
                      (lsp-ui-mode)
                      (setq-local evil-lookup-func 'lsp-describe-thing-at-point))))

(use-package posframe
  :straight t)

(use-package dap-mode
  :straight t
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode)
  :general
  (general-defs
    :keymaps 'prefix-dap-map
    "d" 'dap-hydra)
  :init
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions tooltip)))

(use-package flycheck
  :straight t
  :hook (lsp-mode . flycheck-mode)
  :general
  (local-leader-def
    :keymaps 'flycheck-mode-map
    "[" 'flycheck-list-errors
    ))

(use-package eglot
  :straight t
  :defer t
  :config
  (setq eglot-autoshutdown t)
  (defun eglot-setup () 
    (local-leader-def
      :keymaps 'local
      "." 'eglot-code-actions
      "*" 'eglot-reconnect
      "=" 'eglot-format-buffer))
  (add-hook 'eglot-managed-mode-hook #'eglot-setup)
  )

;; (straight-use-package
;;  '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge" :files ("*")))

;; (straight-use-package
;;  '(acm :type git :host github :repo "manateelazycat/lsp-bridge" :files ("acm/*")))

(use-package epc
  :straight t
  :defer t)


(defun js/disable-company-and-corfu-locally ()
  (company-mode -1)
  (corfu-mode -1))


;; (use-package lsp-bridge
;;   :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
;;             :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
;;             :build (:not compile)))

;; (use-package acm
;;   :straight t
;;   :defer t)

;; (use-package lsp-bridge
;;   :straight t
;;   :defer t
;;   :general
;;   (local-leader-def
;;     :definer 'minor-mode
;;     :keymaps '(lsp-bridge-mode)
;;     "." 'lsp-bridge-code-action
;;     "r" 'lsp-bridge-rename
;;     "*" 'lsp-bridge-restart-process
;;     "=" 'lsp-bridge-code-format)
;;   :config
;;   (defhydra lsp-bridge-diagnostics-hydra ()
;;     ("[" lsp-bridge-jump-to-prev-diagnostic "next")
;;     ("]" lsp-bridge-jump-to-next-diagnostic "prev")
;;     )

;;   (local-leader-def
;;     :definer 'minor-mode
;;     :keymaps '(lsp-bridge-mode)
;;     "[" 'lsp-bridge-diagnostics-hydra/lsp-bridge-jump-to-prev-diagnostic
;;     "]" 'lsp-bridge-diagnostics-hydra/lsp-bridge-jump-to-next-diagnostic
;;     )
;;   (setq lsp-bridge-lookup-doc-tooltip-border-width 3
;;         lsp-bridge-diagnostic-tooltip-border-width 3)
;;   (defun bridge-user-hook ()
;;     (setq-local evil-lookup-func 'lsp-bridge-lookup-documentation)
;;     (corfu-mode -1)
;;     (company-mode -1))
;;   (add-hook 'lsp-bridge-mode-hook
;;             #'bridge-user-hook))

(provide 'lsp-conf)
