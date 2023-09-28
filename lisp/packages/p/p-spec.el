;; -*- lexical-binding: t; -*-

(require 'eieio)

(cl-defgeneric p-get-name (obj)
  "Generic function to aquire a name for a process")

(cl-defgeneric p-get-buffer (obj)
  "Generic function to aquire a buffer for a process.
Can return a string or a live buffer.")

(cl-defgeneric p-get-invocation (obj)
  "Generic function to get the command invocation. The should return
a list compatible with the `make-process' :command argument.")

(provide 'p-spec)
