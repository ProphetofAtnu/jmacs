;; -*- lexical-binding: t; -*-

(use-package dart-mode
  :straight t
  :defer t
  :mode (("\\.dart\\'" . dart-mode)))

(use-package flutter
  :straight t
  :defer t)

(use-package lsp-dart
    :straight t
    :after (dart-mode)
    :general
    (local-leader-def
        :keymaps '(dart-mode-map)
      "r" 'lsp-dart-dap-flutter-hot-reload
      "R" 'lsp-dart-dap-flutter-hot-restart)
    :config
    (add-hook 'dart-mode-hook 'lsp)
    (setq lsp-dart-sdk-dir "/home/scaggj/tools/flutter/bin/cache/dart-sdk")
    )

(provide 'dart-conf)
