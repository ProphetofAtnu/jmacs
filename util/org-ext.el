;; -*- lexical-binding: t; -*-

;; (with-current-buffer (get-buffer "gm-case-study.org")
;;   (goto-char (point-min))
;;   (let (match
;;         (regions nil))
;;     (while (setf
;;             match
;;             (text-property-search-forward
;;              'htmlize-link))
;;       (push (list
;;              (cadr (prop-match-value match))
;;              (prop-match-beginning match)
;;              (prop-match-end match))
;;             regions))
;;     (reverse regions)))

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

(provide 'org-ext)
