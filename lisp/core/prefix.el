;;; -*- lexical-binding: t; -*-

(require 'general)
(require 'evil)

(defmacro define-prefix-map (name &rest defs)
  (let ((prefix-cmd (intern (format "prefix-%s-command" name)))
        (prefix-map (intern (format "prefix-%s-map" name))))
    `(general-define-key
      :prefix-command ',prefix-cmd
      :prefix-map ',prefix-map
      ,@defs)))

(defmacro mount-prefix-map (name ident)
  (let ((prefix-cmd (intern (format "prefix-%s-command" name)))
        (prefix-map (intern (format "prefix-%s-map" name))))
    `'(:prefix-command ,prefix-cmd
       :prefix-map ,prefix-map
       :wk ,ident)))


(general-define-key
 :prefix-command 'prefix-buffer-command
 :prefix-map 'prefix-buffer-map
 "b" 'switch-to-buffer
 "d" 'kill-this-buffer
 "x" 'kill-buffer-and-window
 "k" 'kill-extra-buffers)
 

(general-define-key
 :prefix-command 'prefix-file-command
 :prefix-map 'prefix-file-map
 "f" 'find-file
 "o" 'find-file-other-window
 "s" 'save-buffer
 "S" 'evil-write-all
 )
 

(general-define-key
 :prefix-command 'prefix-emacs-command
 :prefix-map 'prefix-emacs-map
 "r" 'restart-emacs
 "q" 'save-buffers-kill-emacs
 "b" 'kill-buffer-and-window
 "f" 'delete-frame
 "p" 'posframe-delete-all)

(general-define-key
 :prefix-command 'prefix-window-command
 :prefix-map 'prefix-window-map
 "t" 'ace-swap-window
 "w" 'ace-window
 "h" 'evil-window-left
 "j" 'evil-window-down
 "k" 'evil-window-up
 "l" 'evil-window-right
 "s" 'evil-window-split
 "v" 'evil-window-vsplit
 "d" 'evil-window-delete
 "H" 'evil-window-move-far-left
 "J" 'evil-window-move-very-bottom
 "K" 'evil-window-move-very-top
 "L" 'evil-window-move-far-right)
 

(define-prefix-map help 
    "v" 'helpful-variable
    "f" 'helpful-callable)

(define-prefix-map search
    "o" 'occur)

(define-prefix-map insert
    "y" 'yas-insert-snippet
    "a" 'yas-new-snippet
    "r" 'insert-register
    "b" 'insert-buffer
    "!" 'auto-insert)

(define-prefix-map project)

(define-prefix-map utility 
    "l" 'list-processes)

(provide 'core/prefix)
