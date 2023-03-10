;; -*- lexical-binding: t; -*-



(use-package yasnippet
  :straight t
  :hook (emacs-startup . yas-global-mode)
  :config
  ;; (general-def
  ;;     'yas-minor-mode-map
  ;;     "SPC"
  ;;   yas-maybe-expand)
  )

(use-package insert-time
  :straight t
  :general
  (:keymaps 'prefix-insert-map
            "t" 'insert-date-time
            "T" 'insert-time))

(use-package string-inflection
  :straight t
  :config
  (general-defs
    :keymaps 'prefix-edit-map
    "i" (js/to-repeatable string-inflection-all-cycle)))

(use-package smartparens
  :straight t
  :hook (emacs-startup
         . smartparens-global-mode)
  :config 
  (require 'smartparens-config)
  (setq sp-highlight-wrap-overlay t
        sp-highlight-pair-overlay nil
        sp-highlight-wrap-tag-overlay t))

(defun js/replace-smartparens-with-electric-insert ()
  (smartparens-mode -1)
  (electric-pair-mode +1))

(use-package origami
  :straight t
  :hook (emacs-startup . global-origami-mode))

(use-package iedit
  :straight t
  :general
  (:keymaps 'isearch-mode-map
            "C-;" 'iedit-mode-from-isearch
            ))

(use-package treemacs
  :straight t
  :defer t
  :general
  (local-leader-def
    :definer 'minor-mode
    :keymaps '(lsp-mode)
    "TAB" 'treemacs
    "C-<tab>" 'treemacs-add-and-display-current-project-exclusively)
  :config
  (setq treemacs-width 35)
  )

(use-package treemacs-evil
  :straight t)


(provide 'global/global-edit-tools)
