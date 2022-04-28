;; -*- lexical-binding: t; -*-


(defun pyel-core-attr (attr this &optional set setnone)
  (cl-remove-if-not
   #'identity
   (list
    (cons :do "attr")
    (cons "attr" attr)
    (cons "this" this)
    (when set (cons "set" set))
    (when setnone
      (cons "setnone" setnone)))))


(defun pyel-core-call (call args kwargs)
  (cl-remove-if-not
   #'identity
   (list
    (cons :do "call")
    (cons "call" call)
    (cons "args" args)
    (cons "kwargs" kwargs))))


(defun pyel-core-dir (it)
  (cl-remove-if-not
   #'identity
   (list
    (cons :do "dir")
    (cons "it" it))))


(defun pyel-core-echo (it)
  (cl-remove-if-not
   #'identity
   (list
    (cons :do "echo")
    (cons "it" it))))


(defun pyel-core-help (&optional describe)
  (cl-remove-if-not
   #'identity
   (list
    (cons :do "help")
    (when describe
      (cons "describe" describe)))))


(defun pyel-core-import-module (module &optional reload)
  (cl-remove-if-not
   #'identity
   (list
    (cons :do "import_module")
    (cons "module" module)
    (when reload
      (cons "reload" reload)))))


(defun pyel-core-make-el ()
  (cl-remove-if-not
   #'identity
   (list (cons :do "make_el"))))


(defun pyel-core-method (method this &optional args kwargs)
  (cl-remove-if-not
   #'identity
   (list
    (cons :do "method")
    (cons "method" method)
    (cons "this" this)
    (when args (cons "args" args))
    (when kwargs
      (cons "kwargs" kwargs)))))


(defun pyel-core-run (code)
  (cl-remove-if-not
   #'identity
   (list
    (cons :do "run")
    (cons "code" code))))


(defun pyel-core-run-file (file)
  (cl-remove-if-not
   #'identity
   (list
    (cons :do "run_file")
    (cons "file" file))))


(defun pyel-core-scope ()
  (cl-remove-if-not
   #'identity
   (list (cons :do "scope"))))


(defun pyel-core-var (var &optional set glob)
  (cl-remove-if-not
   #'identity
   (list
    (cons :do "var")
    (cons "var" var)
    (when set (cons "set" set))
    (when glob (cons "glob" glob)))))

(provide 'pyel-core)

