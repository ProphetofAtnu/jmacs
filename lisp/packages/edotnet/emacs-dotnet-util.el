;; -*- lexical-binding: t; -*-

(require 'cl)
(require 'eieio)
(require 'emacs-dotnet-contracts)

(defconst emacs-dotnet-class-typesym-attribute
  :edn-typename-symbol)

(defconst emacs-dotnet-class-typename-attribute
  :edn-typename-string)

;; (put 'emacs-dotnet-quit emacs-dotnet-class-typesym-attribute 'TEST)
;; (get 'emacs-dotnet-quit emacs-dotnet-class-typesym-attribute)

(defun emacs-dotnet--transform-case (str)
  (let ((case-fold-search nil))
   (substring  (replace-regexp-in-string
		"[A-Z]"
		#'(lambda (x)
		    (concat "-" (downcase x)))
		str
		t nil nil 0) 1)))

(defun emacs-dotnet--reformat-type-symbol (sym)
  (intern-soft (concat "emacs-dotnet-dt-" (emacs-dotnet--transform-case (symbol-name sym)))))

(defun emacs-dotnet--generate-symbol-attributes (sym)
  (let* ((sname (symbol-name sym))
	 (tfs (emacs-dotnet--reformat-type-symbol sym)))
    (put tfs emacs-dotnet-class-typesym-attribute sym)
    (put tfs emacs-dotnet-class-typename-attribute sname)
    ))

(defun emacs-dotnet-is-kernel-event-p (dat)
  (cl-typecase dat
    (emacs-dotnet-dt-kernel-event t)
    (symbol (or (eq 'emacs-dotnet-dt-kernel-event dat)
		(eq 'emacs-dotnet-dt-kernel-event
		    (eieio-class-name
		     (eieio-class-parent
		      dat)))))
    (t nil)))

(defun emacs-dotnet--generate-all-attributes ()
  (cl-loop for sym in emacs-dotnet-dt-kernel-command-types
	   do (emacs-dotnet--generate-symbol-attributes sym))
  (cl-loop for sym in emacs-dotnet-dt-kernel-event-types
	   do (emacs-dotnet--generate-symbol-attributes sym))
  )

(cl-defmethod object-print ((this emacs-dotnet-dt-kernel-event-envelope) &optional strings)
  (with-slots (eventType) this
    (format "#<emacs-dotnet-dt-kernel-event-envelope :eventType %s>" eventType)))

(provide 'emacs-dotnet-util)
