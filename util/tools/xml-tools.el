;; -*- lexical-binding: t; -*-

(require 'xml)
(require 'tlist-tools)

;;; Utility Functions
(defun xml-find-nodes (predicate xml &optional transform)
  (let (nodes
        (tfn (or transform
                 #'identity)))
    (cl-labels
        ((walker (node)
           (when (consp node)
             (if (funcall predicate node)
                 (push (funcall tfn node) nodes))
             (dolist (n (xml-node-children node))
               (walker n)))))
      (walker (car xml))
      (nreverse nodes))))

(provide 'xml-tools)
