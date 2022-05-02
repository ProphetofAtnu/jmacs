;; -*- lexical-binding: t; -*-

(require 'oclosure)
(require 'thunk)
(require 'cl-macs)

(oclosure-define (ctbl
                  (:predicate ctbl-p))
  "A completion table for completing-read and friends")

(oclosure-define (rolling-ctbl
                  :parent ctbl)
  (table :mutable t)
  (metadata :mutable t)
  (on-change :mutable t))


(defun update-rolling-ctable (tbl func)
  (let* ((cv (slot-value tbl 'table))
         (v (if (thunk-evaluated-p cv)
                (thunk-force cv)
              cv)))
    (setf (slot-value tbl 'table)
          (thunk-delay
           (funcall func (if (functionp v)
                             (thunk-force v)
                           v)))))
  (when-let
      ((cf
        (slot-value tbl 'on-change)))
    (funcall cf))
  nil)

(cl-defun create-rolling-ctable (&optional initial-table &key (change nil) (meta nil))
  (oclosure-lambda
      (rolling-ctbl
       (table
        (thunk-delay
         initial-table))
       (metadata meta)
       (on-change change))
    (str pred flag)
    (pcase flag
      (`(boundaries . ,suffix) (cons 'boundaries
                                     (completion-boundaries
                                      str (thunk-force table) pred suffix)))
      ('metadata (list 'metadata metadata))
      ('lambda (test-completion str (thunk-force table) pred))
      ('nil (try-completion str (thunk-force table) pred))
      (_ (all-completions str (thunk-force table) pred))
      )))


(provide 'ctbl)
