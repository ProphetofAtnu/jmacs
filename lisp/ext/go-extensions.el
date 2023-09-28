;; -*- lexical-binding: t; -*-

(defvar ext/*go-tool-packages*
  '(;; Go tools
    "golang.org/x/tools/gopls"
    "golang.org/x/tools/cmd/eg"
    "golang.org/x/tools/cmd/stringer"
    "golang.org/x/tools/cmd/godoc"
    "golang.org/x/tools/cmd/goimports"
    "golang.org/x/tools/cmd/gotype"
    "golang.org/x/tools/cmd/toolstash"
    "golang.org/x/tools/cmd/fiximports"
    ;; User tools
    "github.com/rogpeppe/godef"
    "github.com/josharian/impl"))

(defvar ext/*update-procs* nil)

(defvar ext/*update-buffer* nil)

(defun ext//install-go-package (pkg)
  (push
   (make-process
    :name (format "go install %s@latest" pkg)
    :command (list "go" "install" pkg)
    :connection-type 'pipe
    :buffer (get-buffer-create "* go install output *")
    )
   ext/*update-procs*))

(provide 'go-extensions)
