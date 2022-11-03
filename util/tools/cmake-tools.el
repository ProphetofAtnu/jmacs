;; -*- lexical-binding: t; -*-

(defvar cmake-comp-cash (make-hash-table :test 'equal))

(defun cmaket--temp-buf ()
  (generate-new-buffer " *cmake-tools*"))

(defun cmaket--build-list-cmd (name)
    (list
     "cmake"
     (format "--help-%s-list" name)))

(defun cmaket--build-doc-cmd (name)
  (let ((actual
         (case name
           ('variable 'variables)
           ('property 'properties)
           ('module 'modules)
           ('command 'commands)
           (t (error "not a documentation list" name)))))
    (list
     "cmake"
     (format "--help-%s" actual))))

(defun cmaket--async-proc (on-finish &rest proc-args)
  (apply #'make-process
         :sentinel (lambda (p msg)
                     (if (process-live-p p)
                         nil
                       (funcall on-finish p)))
         proc-args))

(defun cmaket--line-before ()
  (save-excursion
    (line-move -1 t)
    (buffer-substring-no-properties (point-at-bol)
                                    (point-at-eol))))

(defun cmaket--get-section-marks ()
  (let ((marks nil))
    (save-excursion
      (goto-char (point-min))
      (while 
          (re-search-forward "^-+$" nil t)
        (save-excursion 
          (line-move -1)
          (push
           (point-at-bol) marks))))
    (nreverse marks)))

(defun cmaket--sections-to-ranges (marks)
  (cl-loop for st in marks 
        for se on marks 
        collect (cons st
                      (or (car (cdr-safe se))
                          (point-max)))))

(defun cmaket--read-doc-hash ()
  (let ((ht (make-hash-table :test 'equal)))
    (cl-loop for (s . e) in (cmaket--sections-to-ranges
                             (cmaket--get-section-marks))
          do (progn
               (goto-char s)
               (puthash (string-trim
                         (buffer-substring-no-properties
                          s (point-at-eol)))
                        (string-trim
                         (buffer-substring-no-properties s e))
                        ht)
               ))
    ht))


(defconst cmaket--sig-rx-placeholder
  (rx (seq symbol-start "__PLACEHOLDER__" (* space) "(" (* (not ")")) ")")))

(defun cmaket--extract-signatures (name string)
  (let ((re (string-replace "__PLACEHOLDER__"
                            name cmaket--sig-rx-placeholder))
        (offset 0)
        (sigs nil))
    (print re)
    (while (setf offset (string-match re string offset))
      (push (match-string 0 string) sigs)
      (cl-incf offset))
    sigs))


(defun cmake-load-list (list-name)
  (cl-assert (symbolp list-name))
  (cmaket--async-proc
   (lambda (p)
     (with-current-buffer (process-buffer p)
       (goto-char (point-min))
       (condition-case nil 
           (while t
             (puthash (buffer-substring (point-at-bol) (point-at-eol))
                      (list :kind list-name)
                      cmake-comp-cash)
             (line-move 1))
         ('error nil))
       (kill-buffer)))
   :command (cmaket--build-list-cmd list-name)
   :buffer (cmaket--temp-buf)
   :name "cmaket--list-proc"))

(defun cmake-load-docs (for-name)
  (cl-assert (symbolp for-name))
  (cmaket--async-proc
   (lambda (p)
     (with-current-buffer (process-buffer p)
      (kill-buffer)))
   :command (cmaket--build-list-cmd list-name)
   :buffer (cmaket--temp-buf)
   :name "cmaket--docs-proc"))

(defun cmake-load-all-lists ()
  (dolist (li '(command property variable module))
    (cmake-load-list li)))



(provide 'cmake-tools)

