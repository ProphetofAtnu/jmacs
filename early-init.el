;; -*- lexical-binding: t; -*-

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)

(setq
 gc-cons-threshold 100000000
 read-process-output-max (* 1024 1024))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
