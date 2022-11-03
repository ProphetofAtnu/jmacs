;;; -*- lexical-binding: t; -*-

(use-package yaml-mode
  :straight t
  :mode "\\.\\(e?ya?\\|ra\\)ml\\'")

(use-package docker-compose-mode
  :straight t
  :mode "docker-compose[^/]*\\.ya?ml\\'")

(use-package dockerfile-mode
  :straight t
  :mode (("\\.dockerfile\\'" . dockerfile-mode)
         ("/Dockerfile\\(?:\\.[^/\\]*\\)?\\'" . dockerfile-mode)))


(provide 'docker-conf)
