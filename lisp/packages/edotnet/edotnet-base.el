;; -*- lexical-binding: t; -*-

(defmacro gethash-mult (ht &rest keys)
  `(list ,@(mapcar (lambda (k) `(gethash ,k ht)) keys)))

(defun edn-compose (&rest funcs)
  (lambda (arg)
    (let ((co arg))
      (dolist (f funcs)
        (setf co (funcall f co)))
      co)))

(defun edn--case-apply (str)
  (declare (cl-optimize (speed 3) (safety 0)) (pure t))
  (let ((case-fold-search nil))
    (concat
     (downcase (char-to-string (aref str 0)))
     (downcase (replace-regexp-in-string
                "[[:upper:]]" "-\\&" str t nil nil 1)))
    ))

(defun edn--case-revert (str &optional first-is-upper)
  (declare (cl-optimize (speed 3) (safety 0)) (pure t))
  (cl-loop for posn = (string-search "-" str 0) then (string-search "-" str (1+ posn))
           with pzl = 0
           while posn
           concat (prog1 (funcall (if (and (not first-is-upper) (zerop pzl))
                                      #'identity
                                    #'capitalize)
                                  (substring str pzl posn))
                    (setf pzl (1+ posn))) into track
           finally (return (concat track (funcall (if (and (not first-is-upper) (zerop pzl))
                                                      #'identity
                                                    #'capitalize)
                                                  (substring str pzl))))))

(cl-defstruct (edn-envelope-struct
               (:constructor nil)
               (:copier nil))
  token id routing-slip)

(cl-defstruct (edn-command-struct
               (:include edn-envelope))
  command-type command)

(cl-defstruct (edn-event-struct
               (:include edn-envelope))
  event-type event)

(defmacro edn--defmarshaller (struct)
  (let* ((fname (intern (format "edn-marshaller--%s" struct)))
         (slots
          (thread-last
            struct
            (cl-struct-slot-info)
            (mapcar #'car)
            cdr))
         (keys
          (mapcar
           (edn-compose
            #'symbol-name
            #'edn--case-revert
            #'intern)
           slots))
         (zipped
          (cl-loop for k in keys
                   for acc in slots
                   collect `(let ((v (aref
                                      arg
                                      ,(cl-struct-slot-offset
                                        struct
                                        acc))))
                              (and v (cons ',k v))))))
    `(defun ,fname (arg)
       (declare (pure t) (side-effect-free t)
                (cl-optimize (speed 3)))
       (-non-nil
        (list
         ,@zipped)))))

(defmacro edn--defunmarshaller (struct)
  (let* ((fname (intern (format "edn-unmarshaller--%s" struct)))
         (slots
          (thread-last
            struct
            (cl-struct-slot-info)
            (mapcar #'car)
            cdr))
         (keys
          (mapcar
           (edn-compose
            #'symbol-name
            #'edn--case-revert
            #'intern)
           slots))
         (zipped
          (cl-loop for k in keys
                   for acc in slots
                   collect `(gethash ',k arg nil))))
    `(defun ,fname (arg)
       (declare (pure t) (side-effect-free t)
                (cl-optimize (speed 3)))
       (record ',struct ,@zipped))))

(defmacro edn--make-serializable (struct)
  `(progn 
     (edn--defmarshaller ,struct)
     (edn--defunmarshaller ,struct)))

(defun edn-event-struct-get-symbol (struct)
  (declare (pure t) (side-effect-free t))
  (intern-soft (edn-event-struct-event-type struct)))

(defun edn-type-symbol (struct)
  (cl-case (aref struct 0)
    (edn-command-struct (edn-command-struct-command-type struct))
    (edn-event-struct (edn-event-struct-get-symbol struct))))

(defsubst edn-read-event-string (str)
  (let ((json-object-type 'hash-table)
	(json-key-type 'symbol)
	(json-array-type 'list))
    (edn-unmarshaller--edn-event-struct (json-read-from-string str))))

(defsubst edn-make-command-string (cmd)
  (json-serialize (edn-marshaller--edn-command-struct cmd)))

(provide 'edotnet-base)
