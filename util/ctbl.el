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

(oclosure-define merged-ctable
  tables
  (metadata :mutable t)
  (completions :mutable t))

(defun merged-ctbl--htcreate (tbls)
  (let ((ht (make-hash-table :test 'equal :size 5000))
        (ctr 0))
    (mapc (lambda (tbl)
            (cl-loop for cd in (all-completions "" tbl nil)
                  do (puthash cd ctr ht))
            (cl-incf ctr))
          tbls)
    ht))

(defun merge-ctables (tbls)
  (oclosure-lambda
      (merged-ctable
       (tables tbls)
       (metadata nil)
       (completions
        (thunk-delay (merged-ctbl--htcreate tbls))))
      (str pred flag)

    (pcase flag 
      (`(boundaries . ,suffix) (cons 'boundaries
                                     (completion-boundaries
                                      str (thunk-force completions) pred suffix)))
      ('metadata (list 'metadata metadata))
      ('lambda (test-completion str (thunk-force completions) pred))
      ('nil (try-completion str (thunk-force completions) pred))
      (_ (all-completions str (thunk-force completions) pred))
      )))

(defun merged-ctables-comps (mct)
  (thunk-force (oref mct completions)))

(defun merged-capf--annotator (merged)
  (lambda (comp)
    (when-let ((idx (gethash comp (merged-ctables-comps merged))))
      (with-slots (metadata) merged
        (when-let ((af (plist-get (nth idx metadata) :annotation-function)))
          (funcall af comp))
        ))))

(defun merged-capf--selector (merged ref key)
  (lambda (comp)
    (when-let* ((idx (gethash comp (merged-ctables-comps merged)))
                (fun (aref ref idx)))
      (funcall fun comp))))

(defun merged-capf--merge-plists (merge plists)
  (let ((dist (make-hash-table))
        (len (length plists))
        (ctr 0))
    (dolist (i plists)
      (cl-loop for (k v) on i by #'cddr
            do (let ((gh (gethash k dist (make-vector len nil))))
                 (setf (aref gh ctr) v)
                 (puthash k gh dist)))
      (cl-incf ctr))
    (cl-loop for x being the hash-keys of dist using (hash-values y)
          append (list x (merged-capf--selector merge y x)))))

(defun merge-capfs (&rest capfs)
  (lambda ()
    (let (start end ctables annot merged)
      (cl-loop for (b e tbl . rest) in (mapcar #'funcall capfs)
            when b
            do (progn (setf start b)
                      (setf end e)
                      (push tbl ctables)
                      (push rest annot))
            )
      (when start
        (setf merged (merge-ctables ctables))
        (setf (oref merged metadata) annot)
        (append (list start end merged) (merged-capf--merge-plists merged annot))))))

(provide 'ctbl)

;; (let ((dist (make-hash-table))
;;       (len (length *test*))
;;       (ctr 0))
;;   (dolist (i *test*)
;;     (cl-loop for (k v) on i by #'cddr
;;           do (let ((gh (gethash k dist (make-vector len nil))))
;;                (setf (aref gh ctr) v)
;;                (puthash k gh dist)))
;;     (cl-incf ctr))
;;   (cl-loop for x being the hash-keys of dist using (hash-values y)
;;         append (list x y)))
