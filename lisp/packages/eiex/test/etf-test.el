;;; -*- lexical-binding: t; -*-


;; Test etf-convert-vector-int
(cl-loop for (res args) in etf--test-ints
      for ans = (etf-convert-vector-int args)
      do (cl-assert (= ans res))
      collect (cons ans res)))

(cl-loop for (args res) in etf--test-ints
      for ans = (etf-convert-int-vector args)
      do (cl-assert (equal ans res))
      collect (cons ans res))


