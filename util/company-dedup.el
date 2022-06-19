;; -*- lexical-binding: t; -*-

(defun company-dedup-candidates (candidates)
  (let ((ht (make-hash-table :test 'equal)))
    (cl-loop for c in candidates
          unless (gethash c ht)
          do (puthash c t ht))
    (hash-table-keys ht)))
    
(provide 'company-dedup)
