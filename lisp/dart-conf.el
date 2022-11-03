;; -*- lexical-binding: t; -*-

(use-package dart-mode
    :straight t)

(use-package flutter
    :straight t)

(use-package lsp-dart
    :straight t
    :general
    (local-leader-def
        :keymaps '(dart-mode-map)
      "r" 'lsp-dart-dap-flutter-hot-reload
      "R" 'lsp-dart-dap-flutter-hot-restart)
    :config
    (setq lsp-dart-sdk-dir "/home/scaggj/git/install/flutter/bin/cache/dart-sdk")
    )

(use-package lsp-mode
    :straight t
    :hook (dart-mode . lsp))

(provide 'dart-conf)
