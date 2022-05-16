;; -*- lexical-binding: t; -*-
(require 'epy)

(defgroup epy-mode nil
  "An interactive interface for epy")

(defvar-local epy-mode--buffer-source nil)
(defvar epy-using-sessions nil)

(defvar epy-mode-flush-type #'epy-run-flush-direct-on-new-messages
  "The default flush type for epy-mode buffers.")

(defun epy-capf-annotation-func (item)
  (when-let ((p (get-text-property 0 'py-kind item)))
    (format "   %s" p)))

(defun epy-completion-table-with-cb ()
  (let* ((cline (1+ (current-line)))
         (ccol (current-column))
         (buff (buffer-substring-no-properties (point-min) (point-max)))
         (data nil)
         (waiting t)
         (done-cb
           (lambda (result)
             (setf
              data (cl-loop for (cstr desc) in result
                         collect (propertize cstr 'py-kind desc))
              )
             (setq waiting nil))))
    (run-with-timer 0 nil #'epy-send
                    epy-mode--buffer-source (list
                                     :endpoint "code"
                                     :method "capf"
                                     :args (list buff cline ccol))
                    done-cb)
    (lambda (str)
      (while-no-input
        (when waiting
          (accept-process-output (oref epy-mode--buffer-source proc) 0 500 t)))
      data)))



(defun epy-capf-local-bounds ()
    (save-excursion
     (let ((beg (progn
                  (skip-syntax-backward "w_")
                  (point)))
           (end (progn (skip-syntax-forward "w_")
                       (point))))
       (cons beg end))))

(defun epy-completion-at-point ()
  (when epy-mode--buffer-source
    (-let (((beg . end) (epy-capf-local-bounds)) 
           (ct (completion-table-dynamic (epy-completion-table-with-cb))))
      (list beg end ct
            :annotation-function #'epy-capf-annotation-func))))

(define-minor-mode epy-minor-mode
    "A minor mode for interactive python development and execution."
  :lighter nil
  :group 'epy-mode
  (if epy-minor-mode
      (progn
        (setq epy-mode--buffer-source
              (epy-process :on-message
                           epy-mode-flush-type))
        (add-hook 'completion-at-point-functions #'epy-completion-at-point nil t))
    (progn
      (when epy-mode--buffer-source
        (with-slots (proc) epy-mode--buffer-source
          (kill-process proc))))))


(provide 'epy-mode)
