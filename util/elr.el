;; -*- lexical-binding: t; -*-

(defun elr--avy-select-outer-sexp ()
  (avy-process
   (mapcar
    (lambda (x) (cons x (1+ x)))
    (nth 9 (syntax-ppss)))))

(defvar elr-placeholder-keymap (let ((km (make-sparse-keymap)))
                                   km))

(defvar elr-refactor-region-map (let ((km (make-sparse-keymap)))
                                    (keymap-set km "C-r" nil)
                                    km))
(defun elr--region ()
  (when (get-text-property (point) :elr-region)
    (save-excursion
      (let ((b (previous-single-property-change (point) :elr-region))
            (e (next-single-property-change (point) :elr-region)))
        (cons (or b (point-min)) (or e (point-max)))))))

(defun elr--placeholder (placeholder &optional ord)
  (propertize placeholder
              :elr-placeholder (or ord t)
              'keymap nil))

(defun elr--cleanup-placeholders ()
  (remove-text-properties
   (point-min)
   (point-max)
   '(:elr-placeholder nil)))

(defun elr--generate-let (form)
  (format  "(let ((%s %S))\n)" (elr--placeholder "var") form))

(defun elr-wrap-let ()
  (interactive)
  (let ((sel (elr--avy-select-outer-sexp)))
    (when (consp sel)
      (save-excursion
        (goto-char (car sel))
        (let ((cp (point))
              (form (read (current-buffer))))
          (delete-region cp (point))
          (insert (elr--generate-let form))
          ))
      (indent-pp-sexp))))

(defun elr-inline-defun (func)
  (interactive "a")
  (when-let*  ((pos (find-function-noselect func))
               (buffer (car pos))
               (pnt (cdr pos))
               (defn (with-current-buffer buffer
                       (save-excursion
                         (goto-char pnt)
                         (read (current-buffer))))))
    (pp defn (current-buffer))))

(provide 'elr)
