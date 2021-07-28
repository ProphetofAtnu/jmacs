;;; -*- lexical-binding: t; -*-

(require 'general)
(require 'core/prefix)

(general-create-definer global-leader-def
    :states '(normal motion insert emacs override)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

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
  "h" (mount-prefix-map help "Help")
  "s" (mount-prefix-map search "Search"))

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
