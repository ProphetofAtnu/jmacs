;; -*- lexical-binding: t; -*-

(defvar evil-want-integration t)
(defvar evil-disable-insert-state-bindings t)
(defvar evil-want-keybinding nil)

(use-package evil 
  :delight
  :hook (emacs-startup . evil-mode)
  :init 
  (setq evil-want-C-u-scroll t
        evil-cross-lines t
        evil-undo-system 'undo-redo
        evil-ex-search-vim-style-regexp t
        evil-search-module 'evil-search))

(use-package evil-collection
  :straight t
  :init
  (evil-collection-init)
  (global-evil-collection-unimpaired-mode +1)
  :config
  (delq 'lispy evil-collection-mode-list))

(use-package evil-goggles
  :straight t
  :delight
  :hook (evil-mode . evil-goggles-mode)
  :config
  (evil-goggles-use-diff-faces))

(use-package evil-surround
  :straight t
  :after (evil)
  :commands
  (evil-surround-edit
   evil-Surround-edit
   evil-surround-region
   evil-Surround-region)
  :init
  (evil-define-key 'operator global-map "s" 'evil-surround-edit)
  (evil-define-key 'operator global-map "S" 'evil-surround-edit)
  (evil-define-key 'visual global-map "s" 'evil-surround-region)
  (evil-define-key 'visual global-map "S" 'evil-Surround-region))

(use-package evil-matchit
  :straight t
  :hook (evil-mode . global-evil-matchit-mode))

(use-package evil-lion
  :straight t
  :hook (evil-mode . evil-lion-mode))

(use-package evil-commentary
  :straight t
  :after (evil)
  :bind
  (:map evil-normal-state-map)
  ("gc" . evil-commentary))

(use-package evil-better-visual-line
  :straight t
  :defer 5
  :config
  (add-hook 'visual-line-mode-hook #'evil-better-visual-line-on))

(use-package evil-mc
  :straight t
  :defer nil
  :hook (evil-mode . global-evil-mc-mode)
  :general
  (:keymaps '(evil-mc-key-map)
            :states '(normal)
            "z ." 'evil-mc-make-cursor-here 
            "z <" 'evil-mc-pause-cursors
            "z >" 'evil-mc-resume-cursors
            "z /" 'evil-mc-undo-all-cursors
            )
  :init
  (require 'core/hydras)
  (general-def
    :states '(normal)
    "g.." 'evil-mc-hydra/body)
  (evil-mc-define-vars)
  (add-to-list 'evil-mc-incompatible-minor-modes 'lispy)
  ) 


(use-package evil-avy
  :straight t
  :after (evil)
  ;; :hook (evil-mode . evil-avy-mode)
  :general
  (:states '(normal)
           "s" 'evil-avy-goto-char-2))

(use-package evil-snipe
  :straight t
  :config
  (setq evil-snipe-scope 'whole-buffer
        evil-snipe-enable-highlight t
        evil-snipe-enable-incremental-highlight t
        evil-snipe-show-prompt nil
        evil-snipe-smart-case t)
  (evil-snipe-mode 1))

(provide 'global/global-evil)
