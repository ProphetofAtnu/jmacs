;;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'general)
(require 'evil)

;; (use-package paredit
;;     :straight t
;;     :hook ((emacs-lisp-mode
;;             clojure-mode
;;             lisp-mode)
;;            . paredit-mode)
;;     :general
;;     (:keymaps '(paredit-mode-map)
;;               :states '(normal)
;;               "z (" 'paredit-wrap-round
;;               "z [" 'paredit-wrap-square
;;               "z {" 'paredit-wrap-curly
;;               "M-)" 'paredit-forward-slurp-sexp
;;               "M-(" 'paredit-forward-barf-sexp
;;               ))


;; (use-package parinfer
;;     :straight t
;;     :general
;;     (:keymaps 'parinfer-mode-map
;;               "S-<tab>" 'parinfer-smart-tab:dwim-left
;;               "<tab>" 'parinfer-smart-tab:dwim-right-or-complete)
;;     (local-leader-def
;;         :keymaps '(lisp-mode-shared-map)
;;       "p" 'parinfer-toggle-mode)
;;     :init
;;     (progn
;;       (setq parinfer-extensions
;;             '(defaults       ; should be included.
;;               pretty-parens  ; different paren styles for different modes.
;;               evil
;;               paredit
;;               smart-yank))   ; Yank behavior depend on mode.
;;       (add-hook 'clojure-mode-hook #'parinfer-mode)
;;       (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
;;       (add-hook 'common-lisp-mode-hook #'parinfer-mode)
;;       (add-hook 'scheme-mode-hook #'parinfer-mode)
;;       (add-hook 'lisp-mode-hook #'parinfer-mode)))

(use-package lispy
    :straight t
    ;; :hook ((emacs-lisp-mode
    ;;         lisp-mode
    ;;         clojure-mode
    ;;         common-lisp-mode
    ;;         scheme-mode)
    ;;        . lispy-mode)
    :config
    (setq lispy-completion-method 'default
          lispy-close-quotes-at-end-p t
          lispy-colon-p nil)
    (add-to-list 'lispy-compat 'cider)
    (add-to-list 'lispy-compat 'macrostep))

(use-package lispyville
    :straight t
    :hook (lispy-mode . lispyville-mode))


(defun symex-show-bindings ()
  (interactive)
  (let ((buf (get-buffer-create " *bindings*"))
        (km (make-sparse-keymap)))
    (with-current-buffer buf
      (erase-buffer)
      (insert 
       (cl-loop for x being the key-seqs of symex-editing-mode-map using (key-bindings y)
             when (eq (aref x 0 ) 'symex-state)
             collect (format "%s -> %S" (key-description (cl-subseq x 1)) y) into strings 
             finally (return
                       (string-join (mapcar (lambda (x) (string-join x "\t"))(seq-partition strings 3))
                                    "\n"))))
      (align-regexp (point-min) (point-max) "\\(^\\|\\s-*\\)\t" 1 1 t))
    (posframe-show buf 
                   :poshandler #'posframe-poshandler-frame-top-center)
    (set-transient-map nil nil #'posframe-hide-all)))

(use-package symex
    :straight t
    :general
    (:keymaps '(emacs-lisp-mode-map
                lisp-interaction-mode-map
                lisp-mode-map
                clojure-mode-map
                common-lisp-mode-map
                ielm-mode-map
                cider-repl-mode-map
                scheme-mode-map)
              "C-\\" 'symex-mode-interface)
    :config
    (symex-initialize))

(provide 'sexp-conf)

