;; -*- lexical-binding: t; -*-

(require 'cl)
(require 'emacs-dotnet-contracts)

(cl-defgeneric edn-serialize (target)
  (declare (speed 3) (pure t))
  target)

(cl-defmethod edn-serialize ((target list))
  (declare (speed 3) (pure t))
  (cl-loop for tgt in target
           collect (edn-serialize tgt)))

(eval-when-compile
 (defmacro edn--agen-serializer (clz)
   `(cl-defmethod edn-serialize ((target ,clz))
      (declare (speed 3) (pure t))
      (let ((args nil))
	,@(cl-loop for slt in 
                   (eieio-class-slots clz)
                   collect `(when (and
				   (slot-boundp target ',(oref slt name)))
			      (push (edn-serialize (oref target ,(oref slt name))) args)
                              (push ',(intern-soft (concat ":" (symbol-name (oref slt name)))) args)))
	args)))

 (defmacro emacs-dotnet-generate-serializer (clz)
   `(edn--agen-serializer ,clz))

 (defmacro emacs-dotnet--generate-all-serializers-macro ()
   `(progn
      ,@(cl-loop for clz in emacs-dotnet-dt-classes
                 collect `(emacs-dotnet-generate-serializer ,clz)))))

(emacs-dotnet--generate-all-serializers-macro)

(provide 'emacs-dotnet-serializers)
