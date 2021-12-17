;;; -*- lexical-binding: t; -*-


(rx-define ex-string-delim 
  (and
        ;; Match even number of backslashes.
        (or (not (any ?\\ ?\' ?\")) point
            ;; Quotes might be preceded by escaped quote
            (and (or (not (any ?\\)) point) ?\\
                 (* ?\\ ?\\) (any ?\' ?\")))
        (* ?\\ ?\\)
        ;; Match single or triple quotes of any kind.
        (group (or "\"" "\"\"\"" "'" "'''"))))

(rx-define ex-atom 
  (seq ":"
      (or
       (and
        (any "a-z" "A-Z" "_" "\"" "'")
        (zero-or-more (any "a-z" "A-Z" "0-9" "_" "\"" "'" "!" "@" "?")))
       (and "\"" (one-or-more (not (any "\""))) "\"")
       (and "'" (one-or-more (not (any "'"))) "'"))))

(rx-define ex-num 
  (and symbol-start
   (? "-")
   (+ digit)
   (0+ (and "_" (= 3 digit)))
   symbol-end))

(rx-define ex-builtin 
  (seq symbol-start
      (or "case" "cond" "for" "if" "quote" "raise" "receive" "send"
          "super" "throw" "try" "unless" "unquote" "unquote_splicing"
          "with")
      symbol-end))

(rx-define ex-builtin-decl 
  (seq symbol-start
       (or "def" "defp" "defmodule" "defprotocol"
           "defmacro" "defmacrop" "defdelegate"
           "defexception" "defstruct" "defimpl"
           "defguard" "defguardp" "defcallback"
           "defoverridable")
       symbol-end))

(rx-define ex-ns 
  (seq symbol-start
       (or "import" "require" "use" "alias")
       symbol-end))

;; Set aside code point syntax for negation face.
(rx-define ex-code-point 
  (seq symbol-start
      "?"
      anything
      symbol-end))

(rx-define ex-fun-start 
  (seq (or line-start (not (any ".")))
      symbol-start
      (or "def" "defp")
      symbol-end))

(rx-define ex-kw 
    (seq symbol-start
     (or "fn" "do" "end" "after" "else" "rescue" "catch")
     symbol-end))

(rx-define ex-kw-oper 
    (seq symbol-start
     (or "not" "and" "or" "when" "in")
     symbol-end))

;; The first character of an identifier must be a letter or an underscore.
;; After that, they may contain any alphanumeric character + underscore.
;; Additionally, the final character may be either `?' or `!'.

(rx-define ex-ident 
  (seq (any "A-Z" "a-z" "_")
      (zero-or-more (any "A-Z" "a-z" "0-9" "_"))
      (optional (or "?" "!"))))

;; Module and submodule names start with upper case letter. This
;; can then be followed by any combination of alphanumeric chars.
;; In turn, this can be followed by a `.' which begins the notation of
;; a submodule, which follows the same naming pattern of the module.
;; Finally, like other identifiers, it can be terminated with either `?'
;; or `!'.
(rx-define ex-mod 
  (seq symbol-start
      (optional (or "%" "&"))
      (any "A-Z")
      (zero-or-more (any "A-Z" "a-z" "_" "0-9"))
      (zero-or-more
       (and "."
            (any "A-Z" "_")
            (zero-or-more (any "A-Z" "a-z" "_" "0-9"))))
      (optional (or "!" "?"))
      symbol-end))

(rx-define ex-pseudo-var 
  (seq symbol-start
      (optional (or "%" "&"))
      (or "_" "__MODULE__" "__DIR__" "__ENV__" "__CALLER__"
          "__block__" "__aliases__")
      symbol-end))

(rx-define ex-sigils 
  (seq "~" (or "B" "C" "D" "E" "L" "N" "R" "S" "T" "U" "b" "c" "e" "r" "s" "w")))


(defun syntax-bounds (arg)
  (save-excursion
   (list
    (+
     (point)
     (skip-syntax-backward arg))
    (+
     (point)
     (skip-syntax-forward arg)))))

(provide 'elixir-thingatpt)
