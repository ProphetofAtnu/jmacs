;; -*- lexical-binding: t; -*-

(require 'cl-macs)

(defun js/sly--completion-function-wrapper (fn)
  (let ((cache (make-hash-table :test #'equal)))
    (lambda (string pred action)
      (cl-labels ((all
                   ()
                   (let ((probe (gethash string cache :missing)))
                     (if (eq probe :missing)
                         (puthash string (funcall fn string) cache)
                       probe)))
                  (try ()
                       (let ((all (all)))
                         (and (car all)
                              (if (and (null (cdr (car all)))
                                       (string= string (caar all)))
                                  t
                                string)))))
        (pcase action
          ;; identify this to the custom `sly--completion-in-region-function'
          (`sly--identify t)
          ;; identify this to other UI's
          (`metadata '(metadata
                       (display-sort-function . identity)
                       (category . sly-completion)))
          ;; all completions
          (`t (copy-sequence (car (all))))
          ;; try completion
          (`nil (try))
          (`(try-completion . ,point)
           (cons 'try-completion (cons string point)))
          (`(all-completions . ,_point) (cons 'all-completions (copy-sequence (car (all)))))
          (`(boundaries . ,thing)
           (completion-boundaries string (all) pred thing))

          ;; boundaries or any other value
          (_ nil))))))

(provide 'sly-fix)
