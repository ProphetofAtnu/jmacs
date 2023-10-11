;;; -*- lexical-binding: t; -*-

;; (setq gc-cons-threshold most-positive-fixnum)
;; (add-hook 'emacs-startup-hook
;;        (lambda ()
;;          (setq gc-cons-threshold 100000000)))

(defvar use-package-compute-statistics t)
(defvar native-comp-deferred-compilation-deny-list nil)

;; (defvar org--built-in-p)

(require 'early/bootstrap-straight)
;; Set up use-package
(straight-use-package 'use-package)
(require 'use-package)


(use-package early/core)
(use-package early/vars)
(use-package early/packages)
(use-package early/load-paths)
(use-package early/interactive)


;; (byte-recompile-directory (expand-file-name "util" user-emacs-directory) 0)
(use-package core/utility)
;;;; Platform specific setup
(cond
 ((eq system-type 'darwin) (require 'macos))
 (t (progn (set-face-attribute 'default t :font "FiraCode Nerd Font-12")
           (set-face-font 'fixed-pitch "FiraCode Nerd Font-12")
           (set-frame-font "FiraCode Nerd Font-12" nil t))))

;; Global Configurations
(use-package global/global-tools)
(use-package global/global-edit-tools)
(use-package global/global-evil)
(use-package global/global-completion-styles)
(use-package global/global-which-key)
(use-package global/global-tree-sitter)

(use-package window-conf) ;; TODO: Move to global configurations
(use-package sexp-conf)
(use-package emacs-lisp-conf)
(use-package vertico-conf)
(use-package lsp-conf)
(use-package java-conf)
(use-package clojure-conf)
(use-package clisp-conf)
(use-package scheme-conf)
(use-package shell-conf)
(use-package org-conf)
(use-package python-conf)
(use-package docker-conf)
(use-package erlang-conf)
(use-package elixir-conf)
(use-package racket-conf)
(use-package rust-conf)
(use-package c-conf)
(use-package web-conf)
(use-package scala-conf)
(use-package eww-conf)
(use-package swift-conf)
(use-package go-conf)
(use-package ruby-conf)
(use-package sql-conf)
(use-package dotnet-conf)
(use-package ocaml-conf)
(use-package zig-conf)
(use-package latex-conf)
(use-package nim-conf)
(use-package haskell-conf)
(use-package flutter-conf)
(use-package julia-conf)
(use-package utility-conf)

;; Load the global themes
(use-package global/global-theme)

(load custom-file)
(load (expand-file-name "local.el" user-emacs-directory) nil)

(put 'dired-find-alternate-file 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'list-threads 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(scroll-bar-mode -1)
