;; -*- lexical-binding: t; -*-

(use-package orderless
  :straight t
  :ensure t
  :init
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil)
  )

(use-package yasnippet
  :straight t
  :init
  (yas-global-mode))

(use-package tempel
  :straight t
  :bind (("M-+" . tempel-complete)
         ;; Alternative tempel-expand
         ("M-*" . tempel-insert))
  :general
  (tempel-map
   "M-f" 'tempel-next
   "M-n" 'tempel-next
   "M-b" 'tempel-previous
   "M-p" 'tempel-previous)
  :init
  (setq tempel-trigger-prefix ","))

(use-package corfu
  :straight t
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init
  ;; (setq
  ;; corfu-auto t
  ;; corfu-auto-delay 0.2
  ;; corfu-auto-prefix 2
  ;; corfu-quit-no-match t
  ;; corfu-quit-at-boundary t)
  (setq tab-always-indent 'complete)

  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-auto nil)
              (corfu-mode)))
  (defun js/lsp-completion-setup ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  (add-hook 'lsp-mode-hook #'js/lsp-completion-setup)
  (global-corfu-mode)
  (add-hook 'evil-insert-state-exit-hook
            #'corfu-quit))

(use-package cape
  :straight t
  :config ;; Silence the pcomplete capf, no errors or messages!
  ;; (advice-add
  ;;  'pcomplete-completions-at-point
  ;;  :around #'cape-wrap-silent)
  (dolist (i 
           '(evil-ex-elisp-completion-at-point
             evil-ex-command-completion-at-point
             evil-ex-argument-completion-at-point))
    (advice-add
     i
     :around #'cape-wrap-silent))
  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  ;; (advice-add
  ;;  'pcomplete-completions-at-point
  ;;  :around #'cape-wrap-purify)
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package company
  :straight t
  :general
  (:keymaps 'company-active-map
            "TAB" 'company-complete-common-or-cycle
            "<tab>" 'company-complete-common-or-cycle)
  ;; (:states '(insert)
  ;;          "TAB" 'company-indent-or-complete-common
  ;;          "<tab>" 'company-indent-or-complete-common
  ;;          )
  ;; :init
  ;; (add-hook 'emacs-startup-hook
  ;;           'global-company-mode)
  :config
  (add-hook 'company-mode-hook (lambda ()
                                 (corfu-mode -1)
                                 (setq-local tab-always-indent t))
            )
  (defun company-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (company-mode 1)))
  (delq 'company-semantic company-backends)
  (delq 'company-echo-metadata-frontend company-frontends)
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.2))

(provide 'global/global-completion-styles)
