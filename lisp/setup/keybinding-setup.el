;; -*- lexical-binding: t; -*-

(require 'prefix-binding-utils)

;; I'm setting these before evil can even be declared.

(use-package general
  :hook (emacs-startup . general-override-mode)
  :demand t
  ;; :init
  ;; (require 'core/bindings)
  :config
  (with-eval-after-load 'evil
    (general-defs
      :states '(normal motion)
      "M-u" 'universal-argument)
    ))

(defvar keyboard-quit-hook nil)

(defun js/keyboard-quit-advice (&rest x)
  "Advice to add a hook function to 'keyboard-quit'"
  (run-hooks 'keyboard-quit-hook))

(advice-add 'keyboard-quit :before #'js/keyboard-quit-advice)

(general-create-definer global-leader-def
    :states '(normal motion insert emacs override)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

(keymap-global-set "C-M-g" 'keyboard-escape-quit)

(define-prefix-map buffer
 "b" 'switch-to-buffer
 "o" 'switch-window-then-display-buffer
 "d" 'kill-this-buffer
 "x" 'kill-buffer-and-window
 "i" 'ibuffer-other-window
 "v" 'view-buffer
 "V" 'view-buffer-other-window
 "r" 'revert-buffer-quick
 "k" 'kill-extra-buffers)


(define-prefix-map file
 "f" 'find-file
 "o" 'find-file-other-window
 "a" 'auto-revert-mode
 "s" 'save-buffer
 "S" 'evil-write-all
 "j" 'dired-jump-other-window
 "C-r" 'read-only-mode
 )


(define-prefix-map emacs
 "r" 'restart-emacs
 "q" 'save-buffers-kill-emacs
 "b" 'kill-buffer-and-window
 "f" 'delete-frame
 "p" 'posframe-delete-all)

(define-prefix-map window
 "t" 'switch-window-then-swap-buffer
 "w" 'switch-window
 "D" 'switch-window-then-delete
 "h" 'evil-window-left
 "j" 'evil-window-down
 "k" 'evil-window-up
 "l" 'evil-window-right
 "s" 'evil-window-split
 "S" 'switch-window-then-split-vertically
 "v" 'evil-window-vsplit
 "V" 'switch-window-then-split-horizontally
 "d" 'evil-window-delete
 "H" 'evil-window-move-far-left
 "J" 'evil-window-move-very-bottom
 "K" 'evil-window-move-very-top
 "L" 'evil-window-move-far-right)


(define-prefix-map help 
		   "v" 'helpful-variable
		   "f" 'helpful-callable
		   "i" 'info
		   "w" 'which-key-show-major-mode)

(define-prefix-map jump
		   "i" 'ibuffer-jump
		   "d" 'dired-jump
		   "D" 'dired-jump-other-window)

(define-prefix-map search
		   "o" 'occur)

(define-prefix-map insert
		   "y" 'yas-insert-snippet
		   "a" 'yas-new-snippet
		   "r" 'insert-register
		   "b" 'insert-buffer
		   "!" 'auto-insert)

(define-prefix-map frame
		   "o" 'other-frame
		   "n" 'make-frame
		   "m" 'make-frame-on-monitor
		   "d" 'delete-frame
		   "f" 'toggle-frame-maximized)

(define-prefix-map project)

(define-prefix-map modifier
		   "w" 'other-window-prefix
		   "f" 'other-frame-prefix
		   "t" 'other-tab-prefix)

(define-prefix-map utility 
		   "a" 'auto-insert
		   "s" 'scratch-buffer
		   "l" 'list-processes)

(define-prefix-map edit)


(global-leader-def
    :keymaps '(override)
  "" nil
  "SPC" 'execute-extended-command
  "b" '(:prefix-command prefix-buffer-command
        :prefix-map prefix-buffer-map
        :wk "Buffer")
  "f" '(:prefix-command prefix-file-command
        :prefix-map prefix-file-map
        :wk "File")
  "q" '(:prefix-command prefix-emacs-command
        :prefix-map prefix-emacs-map
        :wk "Emacs")
  "w" '(:prefix-command prefix-window-command
        :prefix-map prefix-window-map
        :wk "Window")
  "t" 'dired-jump-in-side-window
  "u" (mount-prefix-map utility "Utility")
  "j" (mount-prefix-map jump "Jump")
  "e" (mount-prefix-map edit "Edit")
  "h" (mount-prefix-map help "Help")
  "o" (mount-prefix-map frame "Frame")
  "s" (mount-prefix-map search "Search")
  "i" (mount-prefix-map insert "Insert")
  "m" (mount-prefix-map modifier "Modifier")
  "v" (mount-prefix-map project "Project"))

(general-create-definer local-leader-def
    :states '(normal motion insert emacs override)
    :prefix ","
    :non-normal-prefix "C-,")

(local-leader-def
    :keymaps '(override)
  "" nil)

(general-defs
    :keymaps '(normal)
  "-" 'dired-jump)

(provide 'setup/keybinding-setup)
