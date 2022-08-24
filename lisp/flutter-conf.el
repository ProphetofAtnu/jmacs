;;; -*- lexical-binding: t; -*-

(use-package dart-mode
    :straight t)

(use-package flutter
    :straight t)

(use-package lsp-dart
    :straight t)

(use-package lsp-mode
    :hook (dart-mode . lsp-deferred))

(provide 'flutter-conf)
