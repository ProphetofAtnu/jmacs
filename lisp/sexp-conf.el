;;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'general)
(require 'evil)

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
  (let ((buf (get-buffer-create " *bindings*")))
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
                   :internal-border-width 1
                   :poshandler #'posframe-poshandler-frame-top-center)
    (set-transient-map nil nil #'posframe-hide-all)))

(defun symex--ext-hl-line ()
  (if symex-editing-mode
      (hl-line-mode +1)
    (hl-line-mode -1)))

(use-package symex
    :straight t
    :custom
    (symex-common-lisp-backend 'sly)
    :general
    (:keymaps '(emacs-lisp-mode-map
                lisp-interaction-mode-map
                lisp-mode-map
                clojure-mode-map
                common-lisp-mode-map
                ielm-map
                cider-repl-mode-map
                scheme-mode-map)
              "C-\\" 'symex-mode-interface)
    :config
    (symex-initialize)
    (add-hook 'symex-editing-mode-hook #'symex--ext-hl-line)
    (setq evil-symex-state-cursor 'hollow)
    (evil-define-key 'symex symex-editing-mode-map "\M-?" #'symex-show-bindings))

(provide 'sexp-conf)

