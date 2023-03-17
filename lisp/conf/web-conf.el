;;; -*- lexical-binding: t; -*-

(use-package js2-mode
    :straight t
    :mode (("\\.js\\'" . js2-mode)
           ("\\.jsx\\'" . js2-jsx-mode))
    :general
    (:keymaps 'js2-mode-map
              :modes 'insert
              "RET" 'js2-line-break))

(use-package typescript-mode
    :straight t)

(defun in-npm-project ()
  (let ((dir default-directory))
    (cl-block nil
      (while (not (equal dir "/.."))
        (when  (member "package.json" (directory-files dir))
          (cl-return t))
        (setf dir (expand-file-name ".." dir))))))

(defun detect-js-workspace-type ()
  (if (in-npm-project)
      (lsp-deferred)
    (progn (setq-local lsp-enabled-clients '(deno-ls))
           (lsp-deferred))))

(use-package lsp-mode
    :straight t
    :hook ((json-mode
            svelte-mode
	    typescript-mode
            js2-jsx-mode)
           . lsp-deferred)
    ;; :init
    ;; (add-hook 'js2-mode-hook 'detect-js-workspace-type)
    ;; (add-hook 'typescript-mode-hook 'detect-js-workspace-type)
    )

(use-package web-mode
    :straight t)

(use-package svelte-mode
    :straight t)

(use-package json-mode
    :straight t)

(use-package emmet-mode
    :straight t
    :hook (html-mode . emmet-mode))

(provide 'web-conf)
