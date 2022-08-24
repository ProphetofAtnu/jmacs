;; -*- lexical-binding: t; -*-

(defvar clike-clang-bin "clang")
(defvar clike-cmake-bin "cmake")

(defvar clike-search-timeout 1)

(defsubst clike-find-file (name &optional in-dir)
  (with-timeout (clike-search-timeout)
    (directory-files-recursively (or in-dir default-directory)
                                 name   
                                 t nil t)))

(defun clike-most-dominating-dir (name &optional in-dir)
  "Find the top level CMakeLists.txt file for the current directory"
  (let ((current
         (locate-dominating-file 
          (or in-dir default-directory)
          name))
        (previous nil))
    (while current
      (setf previous current
            current
            (locate-dominating-file 
             (file-parent-directory current)
             name)))
    previous))


;;;; CMake helpers
(defun clike-cmake-find-lists (&optional in-dir)
  "Find the nearest CMakeLists.txt file"
  (locate-dominating-file (or in-dir default-directory) "CMakeLists.txt"))

(defun clike-cmake-find-root-lists (&optional in-dir)
  "Find the top level CMakeLists.txt file for the current directory"
  (clike-most-dominating-dir "CMakeLists.txt"))

(defvar clike-root-locator-fn #'clike-cmake-find-root-lists)

(defun clike-locate-cmake-cache (&optional in-dir)
  (car
   (clike-find-file "CMakeCache\\.txt$" (or in-dir
                                            (funcall clike-root-locator-fn)))))

(defsubst clike--split-cmake-var (vstr)
  (let ((cln (string-search ":" vstr))
        (eqc (string-search "=" vstr)))
    (unless eqc
      (error "malformed cmake variable"))
    (cons (substring vstr 0 (or cln eqc))
          (substring vstr (1+ eqc)))))

(defsubst clike--buffer-read-cmake-vars-list ()
  (let ((vars (make-hash-table :test 'equal)))
    (goto-char (point-min))
    (while (re-search-forward "^[a-zA-Z0-9].*$" nil t)
      (cl-destructuring-bind (k . v) (clike--split-cmake-var (match-string 0))
            (puthash k v vars)))
    vars))

(defun clike-load-cmake-cache (file)
  (with-temp-buffer
    (insert-file-contents file)
    (clike--buffer-read-cmake-vars-list)))

;;;; Clang helpers
(defun clike-locate-compile-commands (&optional in-dir)
  (car
   (clike-find-file "compile_commands\\.json$"
                    (or in-dir
                        (funcall clike-root-locator-fn)))))

(defun clike-load-compile-commands (file)
  (let ((json-key-type 'keyword)
        (json-array-type 'list))
    (json-read-file file)))

(getenv "INCLUDE_DIRS")

(defun clike--extract-command-info (cmd)
  (let* ((options (split-string cmd "[[:space:]\r\n]" t))
         (cmd (pop options))
         (extra nil)
         (extra-kv nil)
         (sysroot "/")
         (output nil)
         (input nil)
         (includes nil))
    (while options
      (pcase options
        (`(,(rx (seq
                 "-I"
                 (let x (* any)))) . ,rest)
          (progn (push x includes)
                 (setf options rest)))
        (`(,(rx (seq 
                 (let flg (seq "-" (+ (not ?=))))
                 "="
                 (let value (+ any))))
            . ,rest)
          (progn (push (cons flg value) extra-kv)
                 (setf options rest)))
        
        (`("-o" ,val . ,rest)
          (setf output val
                options rest))

        (`("-isysroot" ,val . ,rest)
          (setf sysroot val
           options rest))

        (`(,file . nil)
          (setf
           input file
           options nil))

        (`(,flg . ,rest)
          (progn (push flg extra)
                 (setf options rest))
          )
        ;; Just a fallback. The last rule should catch anything else.
        (_ (pop options))))
    (list 
     :command cmd
     :input input
     :output output
     :include-dirs (nreverse includes)
     :sysroot sysroot
     :other-keys
     (nreverse extra-kv)
     :other (nreverse extra))
    ))


;;;; An example for testing...

;; (clike--extract-command-info
;; "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/cc
;; -Dguileobjc_EXPORTS
;; -I/opt/homebrew/Cellar/guile/3.0.8/include/guile/3.0
;; -I/opt/homebrew/opt/gmp -I/opt/homebrew/opt/gmp/include
;; -I/opt/homebrew/opt/readline/include
;; -I/opt/homebrew/opt/bdw-gc/include -I/Users/jacsc/Code/guileobjc/.
;; -arch arm64 -isysroot
;; /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX12.3.sdk
;; -fPIC -D_THREAD_SAFE
;; -I/opt/homebrew/Cellar/guile/3.0.8/include/guile/3.0
;; -I/opt/homebrew/opt/gmp -I/opt/homebrew/opt/gmp/include
;; -I/opt/homebrew/opt/readline/include
;; -I/opt/homebrew/opt/bdw-gc/include -std=gnu11 -o
;; CMakeFiles/guileobjc.dir/gobjc/objects.m.o -c
;; /Users/jacsc/Code/guileobjc/gobjc/objects.m")

(provide 'clike)
