;;; -*- lexical-binding: t; -*-

(menu-bar-mode +1)

(defvar macos-font "Fira Code-14:antialias=true")

(setq default-frame-alist
      '((ns-transparent-titlebar t)))

(set-frame-parameter (selected-frame) 'ns-transparent-titlebar t)


(set-face-attribute 'default t :font macos-font)
(set-face-font 'fixed-pitch macos-font)
(set-frame-font macos-font nil t)

(progn
  (setq confirm-kill-emacs #'y-or-n-p)
  (setq mac-option-modifier 'meta)
  (setq window-resize-pixelwise t)
  (setq frame-resize-pixelwise t)
  (setq mac-command-modifier 'hyper)
  (setq inferior-lisp-program "sbcl"))

;; Generic Set of typical mac stuff
(global-set-key [(hyper a)] 'mark-whole-buffer)
(global-set-key [(hyper v)] 'yank)
(global-set-key [(hyper c)] 'kill-ring-save)
(global-set-key [(hyper s)] 'save-buffer)
(global-set-key [(hyper f)] 'isearch-forward-regexp)
(global-set-key [(hyper w)]
                (lambda () (interactive) (delete-window)))
(global-set-key [(hyper z)] 'undo)
(global-set-key [(hyper ?`)] 'ns-next-frame)
(global-set-key [(hyper ?\;)] 'eval-expression)
(global-set-key [(hyper t)] 'tab-new)
(global-set-key [(hyper o)] 'other-window-prefix)


;; OCAML LDPath for opam
(setenv "OPAM_SWITCH_PREFIX "  "/Users/jacsc/.opam/default")
(setenv "CAML_LD_LIBRARY_PATH" "/Users/jacsc/.opam/default/lib/stublibs:/Users/jacsc/.opam/default/lib/ocaml/stublibs:/Users/jacsc/.opam/default/lib/ocaml")
(setenv "OCAML_TOPLEVEL_PATH"  "/Users/jacsc/.opam/default/lib/toplevel")

(provide 'macos)
