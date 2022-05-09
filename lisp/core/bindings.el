;;; -*- lexical-binding: t; -*-

(require 'general)
(require 'core/interactive)
(require 'core/prefix)

(defvar keyboard-quit-hook nil)

(defun js/keyboard-quit-advice (&rest x)
  "Advice to add a hook function to 'keyboard-quit'"
  (run-hooks 'keyboard-quit-hook))

(advice-add 'keyboard-quit :before #'js/keyboard-quit-advice)

(general-create-definer global-leader-def
    :states '(normal motion insert emacs override)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")


(global-leader-def
    :keymaps '(override)
  "" nil
  "SPC" 'execute-extended-command
  "g" 'keyboard-escape-quit
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
  "h" (mount-prefix-map help "Help")
  "s" (mount-prefix-map search "Search")
  "i" (mount-prefix-map insert "Insert")
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

(provide 'core/bindings)
