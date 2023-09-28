;; -*- lexical-binding: t; -*-

(require 'cl-macs)

(defmacro eli-do-bound-symbols (spec &rest body)
  (declare (indent 1) (debug ((symbolp &optional form)
                              cl-declarations body)))
  `(cl-do-all-symbols (,(car spec) ,(cdr spec))
     (if (boundp ,(car spec))
         ,@body)))

(defun eli-bound-symbol-count ()
  (let ((cnt 0))
    (eli-do-bound-symbols (v)
      (cl-incf cnt))
    cnt))

(defun eli-plist-symbol-count ()
  (let ((cnt 0))
    (cl-do-all-symbols (v cnt)
      (when (symbol-plist v)
	(cl-incf cnt)))))


(provide 'elisp-introspection)
;; Local Variables:
;; read-symbol-shorthands: (("eli-" . "elisp-introspection-"))
;; End:
