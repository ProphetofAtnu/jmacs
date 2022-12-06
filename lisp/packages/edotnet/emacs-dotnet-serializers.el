;; -*- lexical-binding: t; -*-

(defun emacs-dotnet--plist-encoder (class)
  (cons 'list 
    (cl-loop for slt in 
	     (eieio-class-slots class)
	     nconc `(',(oref slt name) (oref target ,(oref slt name))))))

(defmacro emacs-dotnet--gen-encoder (class)
  (cl-loop for slt in 
	 (eieio-class-slots 'emacs-dotnet-quit)
	 collect `(,(oref slt name) (oref target ,(oref slt name))))

  `())

(cl-loop for slt in 
	 (eieio-class-slots 'emacs-dotnet-quit)
	 collect `(,(oref slt name) (oref target ,(oref slt name))))

(defmacro emacs-dotnet-generate-serializer (class)
  (eieio-class-slots class))


(provide 'emacs-dotnet-serializers)
