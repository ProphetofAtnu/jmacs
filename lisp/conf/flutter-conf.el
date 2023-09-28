;;; -*- lexical-binding: t; -*-

(use-package dart-mode
  :straight t
  :mode ("\\.dart\\'" . dart-mode)
  )

(use-package flutter
  :straight t
  :after (dart-mode))

(use-package lsp-mode
    :hook (dart-mode . lsp-deferred))

(use-package lsp-dart
  :straight t
  :after (lsp-mode))

(provide 'flutter-conf)
