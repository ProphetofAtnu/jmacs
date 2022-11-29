;; -*- mode: lisp -*-
(in-package :common-lisp-user)

(defvar *user-quicklisp-autoload*
  '(:clsql :clsql-odbc
    :alexandria :named-readtables
    :drakma :cl-json :iterate))

(defvar *user-toplevel-packages*
  '(#:iterate
    #:uiop/utility
    #:uiop/os
    #:uiop/pathname
    #:uiop/stream))

(ql:quickload *user-quicklisp-autoload* :silent t)
(use-package *user-toplevel-packages*)

(provide 'user-config)
