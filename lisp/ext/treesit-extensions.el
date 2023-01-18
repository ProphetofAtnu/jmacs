;; -*- lexical-binding: t; -*-

(defun ext/treesit-python-setup ()
  (when (treesit-ready-p 'python)
    (treesit-parser-create 'python)
    (setq-local treesit-font-lock-feature-list
                '(( comment definition)
                  ( keyword string type)
                  ( assignment builtin constant decorator
                    escape-sequence number property string-interpolation )
                  ( bracket delimiter function operator variable)))
    (setq-local treesit-font-lock-settings python--treesit-settings)
    (setq-local imenu-create-index-function
                #'python-imenu-treesit-create-index)
    (setq-local treesit-defun-type-regexp (rx (or "function" "class")
                                              "_definition"))
    (setq-local treesit-defun-name-function
                #'python--treesit-defun-name)
    (treesit-major-mode-setup)))

(defun ext/treesit-c-setup ()
  (when (treesit-ready-p 'c)
    (require 'c-ts-mode)
    (treesit-parser-create 'c)
    (setq-local treesit-font-lock-settings (c-ts-mode--font-lock-settings 'c))
    (treesit-major-mode-setup)))

(defun ext/treesit-c++-setup ()
  (when (treesit-ready-p 'cpp)
    (require 'c-ts-mode)
    (setq-local treesit-text-type-regexp
                (regexp-opt '("comment"
                              "raw_string_literal")))
    (treesit-parser-create 'cpp)
    ;; Syntax.
    (setq-local syntax-propertize-function
                #'c-ts-mode--syntax-propertize)
    (setq-local treesit-font-lock-settings (c-ts-mode--font-lock-settings 'cpp))
    (treesit-major-mode-setup)))

(defun ext/treesit-go-setup ()
  (when (treesit-ready-p 'go)
    (require 'go-ts-mode)
    (treesit-parser-create 'go)

    ;; Navigation.
    (setq-local treesit-defun-type-regexp
                (regexp-opt '("method_declaration"
                              "function_declaration"
                              "type_declaration")))
    (setq-local treesit-defun-name-function #'go-ts-mode--defun-name)

    ;; Imenu.
    (setq-local treesit-simple-imenu-settings
                `(("Function" "\\`function_declaration\\'" nil nil)
                  ("Method" "\\`method_declaration\\'" nil nil)
                  ("Struct" "\\`type_declaration\\'" go-ts-mode--struct-node-p nil)
                  ("Interface" "\\`type_declaration\\'" go-ts-mode--interface-node-p nil)
                  ("Type" "\\`type_declaration\\'" go-ts-mode--other-type-node-p nil)
                  ("Alias" "\\`type_declaration\\'" go-ts-mode--alias-node-p nil)))

    ;; Indent.
    (setq-local indent-tabs-mode t
                treesit-simple-indent-rules go-ts-mode--indent-rules)

    ;; Electric
    (setq-local electric-indent-chars
                (append "{}()" electric-indent-chars))

    ;; Font-lock.
    (setq-local treesit-font-lock-settings go-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '(( comment)
                  ( keyword string type)
                  ( constant escape-sequence function label number
                    property variable)
                  ( bracket delimiter error operator)))

    (treesit-major-mode-setup)))


(defun ext/treesit-bash-setup ()
  (when (treesit-ready-p 'bash)
    (treesit-parser-create 'bash)
    (setq-local treesit-font-lock-feature-list
                '(( comment function)
                  ( command declaration-command keyword string)
                  ( builtin-variable constant heredoc number variable)
                  ( bracket delimiter misc-punctuation operator)))
    (setq-local treesit-font-lock-settings
                sh-mode--treesit-settings)
    (setq-local treesit-text-type-regexp
                (regexp-opt '("comment"
                              "heredoc_start"
                              "heredoc_body")))
    (setq-local treesit-defun-type-regexp "function_definition")
    (treesit-major-mode-setup)))

(defun ext/treesit-yaml-setup ()
  (when (treesit-ready-p 'yaml)
    (treesit-parser-create 'yaml)

    ;; Comments.
    (setq-local comment-start "# ")
    (setq-local comment-end "")

    ;; Indentation.
    (setq-local indent-tabs-mode nil)

    ;; Font-lock.
    (setq-local treesit-font-lock-settings yaml-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment)
                  (string type)
                  (constant escape-sequence number property)
                  (bracket delimiter error misc-punctuation)))

    (treesit-major-mode-setup)))

(defun ext/treesit-rust-setup ()
  (when (treesit-ready-p 'rust)
    (require 'rust-ts-mode)
    (treesit-parser-create 'rust)
    ;; Comments.
    (c-ts-mode-comment-setup)
    ;; Font-lock.
    (setq-local treesit-font-lock-settings rust-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '(( comment)
                  ( keyword string)
                  ( attribute builtin constant escape-sequence
                    function number property type variable)
                  ( bracket delimiter error operator)))

    ;; Imenu.
    (setq-local treesit-simple-imenu-settings
                `(("Module" "\\`mod_item\\'" nil nil)
                  ("Enum" "\\`enum_item\\'" nil nil)
                  ("Impl" "\\`impl_item\\'" nil nil)
                  ("Type" "\\`type_item\\'" nil nil)
                  ("Struct" "\\`struct_item\\'" nil nil)
                  ("Fn" "\\`function_item\\'" nil nil)))

    ;; Indent.
    (setq-local indent-tabs-mode nil
                treesit-simple-indent-rules rust-ts-mode--indent-rules)

    ;; Navigation.
    (setq-local treesit-defun-type-regexp
                (regexp-opt '("enum_item"
                              "function_item"
                              "impl_item"
                              "struct_item")))
    (setq-local treesit-defun-name-function #'rust-ts-mode--defun-name)
    (treesit-major-mode-setup)))

(defun ext/cmake-ts-setup ()
  (when (treesit-ready-p 'cmake)
    (require 'cmake-ts-mode)
    (treesit-parser-create 'cmake)

    ;; Comments.
    ;; Imenu.
    (setq-local imenu-create-index-function #'cmake-ts-mode--imenu)
    (setq-local which-func-functions nil)

    ;; Indent.
    (setq-local treesit-simple-indent-rules cmake-ts-mode--indent-rules)

    ;; Font-lock.
    (setq-local treesit-font-lock-settings cmake-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment)
                  (keyword string)
                  (builtin constant escape-sequence function number variable)
                  (bracket error misc-punctuation)))
    (treesit-major-mode-setup)))

(defun ext/toml-ts-setup ()
  (when (treesit-ready-p 'toml)
    (require 'toml-ts-mode)
    (treesit-parser-create 'toml)
    ;; Comments
    (setq-local comment-start "# ")
    (setq-local comment-end "")

    ;; Indent.
    (setq-local treesit-simple-indent-rules toml-ts-mode--indent-rules)

    ;; Navigation.
    (setq-local treesit-defun-type-regexp
                (rx (or "table" "table_array_element")))
    (setq-local treesit-defun-name-function #'toml-ts-mode--defun-name)

    ;; Font-lock.
    (setq-local treesit-font-lock-settings toml-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment)
                  (constant number pair string)
                  (escape-sequence)
                  (delimiter error)))

    ;; Imenu.
    (setq-local treesit-simple-imenu-settings
                '(("Header" "\\`table\\'" nil nil)
                  ("Array" "\\`table_array_element\\'" nil nil)))

    (treesit-major-mode-setup)))

(provide 'treesit-extensions)
