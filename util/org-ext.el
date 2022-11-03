;; -*- lexical-binding: t; -*-
(require 'dash)
(require 'org)

(defvar org-ext-ctx-handlers
  (let ((ht (make-hash-table)))
    (puthash :link #'org-link-open ht)
    (puthash :checkbox #'org-toggle-checkbox ht)
    (puthash :todo-keyword #'org-todo ht)
    (puthash :priority #'org-priority ht)
    (puthash :headline-stars #'org-cycle ht)
    ht))

(defun org-ext-get-prop-uri (match)
  (if (prop-match-p match)
      (cadr (prop-match-value match))))

(defmacro org-ext-dolinks (match-var link-var &rest body)
  `(save-excursion
     (goto-char (point-min))
     (let (,match-var ,link-var)
       (while (setf
               ,match-var
               (text-property-search-forward
                'htmlize-link)
               ,link-var (org-ext-get-prop-uri ,match-var))
         ,@body))))

(defun org-ext-attachment-data ()
 (org-map-entries (lambda () 
                    (when-let ((adir (org-attach-dir)))
                      (list (org-id-get)
                            adir
                            (directory-files adir nil
                             directory-files-no-dot-files-regexp))))))

(defun org-ext-attach-links-by-entry ()
  (let (udata)
    (org-ext-dolinks
     _
     link
     (when (string-prefix-p
            "attachment:"
            link)
       (push
        (cons (org-id-get)
              (string-remove-prefix
               "attachment:"
               link))
        udata)))
    (mapcar
     (lambda (grp)
       (cons (car grp)
             (mapcar #'cdr (cdr grp))))
     (seq-group-by #'car (reverse udata)))))

(defun org-ext-calculate-attachment-usage ()
  (let ((attach (org-ext-attach-links-by-entry))
        (ldata (org-ext-attachment-data))
        info)
    (pcase-dolist (`(,id . ,refs) attach)
      (let* ((attachments (nth 2 (assoc id ldata #'string=)))
             (common
              (seq-intersection refs attachments #'string=)))
        (print attachments)
        (push (list
               :id id
               :missing-refs (seq-difference refs attachments #'string=)
               :extra-refs (seq-difference attachments refs #'string=))
              info)))
    info))

(define-minor-mode org-headline-emphasize-minor-mode
    "Minor mode that increases the size of org header levels"
    :global t
    :lighter nil
    (if org-headline-emphasize-minor-mode
     (progn
       (set-face-attribute 'org-level-1 nil :height 1.4)
       (set-face-attribute 'org-level-2 nil :height 1.35)
       (set-face-attribute 'org-level-3 nil :height 1.3)
       (set-face-attribute 'org-level-4 nil :height 1.25)
       (set-face-attribute 'org-level-5 nil :height 1.2)
       (set-face-attribute 'org-level-6 nil :height 1.15)
       (set-face-attribute 'org-level-7 nil :height 1.1)
       (set-face-attribute 'org-level-7 nil :height 1.05))
      (progn
       (set-face-attribute 'org-level-1 nil :height 'unspecified)
       (set-face-attribute 'org-level-2 nil :height 'unspecified)
       (set-face-attribute 'org-level-3 nil :height 'unspecified)
       (set-face-attribute 'org-level-4 nil :height 'unspecified)
       (set-face-attribute 'org-level-5 nil :height 'unspecified)
       (set-face-attribute 'org-level-6 nil :height 'unspecified)
       (set-face-attribute 'org-level-7 nil :height 'unspecified)
       (set-face-attribute 'org-level-7 nil :height 'unspecified))))


;; :headline         anywhere in a headline
;; :headline-stars   on the leading stars in a headline
;; :todo-keyword     on a TODO keyword (including DONE) in a headline
;; :tags             on the TAGS in a headline
;; :priority         on the priority cookie in a headline
;; :item             on the first line of a plain list item
;; :item-bullet      on the bullet/number of a plain list item
;; :checkbox         on the checkbox in a plain list item
;; :table            in an Org table
;; :table-special    on a special filed in a table
;; :table-table      in a table.el table
;; :clocktable       in a clocktable
;; :src-block        in a source block
;; :link             on a hyperlink
;; :keyword          on a keyword: SCHEDULED, DEADLINE, CLOSE, COMMENT.
;; :latex-fragment   on a LaTeX fragment
;; :latex-preview    on a LaTeX fragment with overlaid preview image

(dolist (osm
          '(:link :todo-keyword :checkbox :tags :headline-stars
            :table-special :priority :src-block :latex-fragment :latex-preview))
  (put osm 'org-ext-prio 10))

(dolist (osm
          '(:keyword :headline :item :item-bullet :table-table :table :clocktable))
  (put osm 'org-ext-prio 0))

(defun org-ext--prio-sorter (sym1 sym2)
  (let ((p1 (get sym1 'org-ext-prio))
        (p2 (get sym2 'org-ext-prio)))
    (> (or p1 -1) (or p2 -1))))

(defun org-ext--context-prioritize ()
  (sort
   (mapcar #'car (org-context))
   #'org-ext--prio-sorter))

(defun org-ext-dynamic-ret ()
  (interactive)
  (let ((ctx (org-ext--context-prioritize)))
    (if-let ((hndlr (cl-block nil
                      (dolist (c ctx)
                        (when-let ((hndlr (gethash c org-ext-ctx-handlers)))
                          (cl-return hndlr))))))
          (funcall-interactively hndlr)
      nil)))

(provide 'org-ext)
