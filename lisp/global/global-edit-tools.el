;; -*- lexical-binding: t; -*-


(use-package tempel
  :straight t
  :bind (("M-+" . tempel-complete)
         ;; Alternative tempel-expand
         ("M-*" . tempel-insert)))


(use-package yasnippet
  :straight t
  :hook (emacs-startup . yas-global-mode)
  :config
  ;; (general-def
  ;;     'yas-minor-mode-map
  ;;     "SPC"
  ;;   yas-maybe-expand)
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand))

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

(use-package origami
  :straight t
  :hook (emacs-startup . global-origami-mode))

(use-package iedit
  :straight t
  :general
  (:keymaps 'isearch-mode-map
            "C-;" 'iedit-mode-from-isearch
            ))


(provide 'global/global-edit-tools)
