;;; -*- lexical-binding: t; -*-

(require 'url)

(defvar jsonlib-schemastore-url "https://www.schemastore.org/api/json/catalog.json"
  "The url to use for getting the SchemaStore listing")

(defun jsonlib-get-schemastore-listing nil
  (jsonlib-request jsonlib-schemastore-url))

(defun jsonlib-request (url &optional
                              object-type
                              array-type)
  (let ((objs (or object-type 'alist))
        (arrs (or array-type 'array)))
    (with-current-buffer (get-buffer-create " *json-schema-response*")
      (url-insert-file-contents url)
      (prog1
          (json-parse-buffer :object-type objs :array-type arrs)
        (kill-buffer)))))

(defconst jsonlib-types
  '(null 
    boolean 
    object 
    array 
    number 
    string 
    integer))

(defun jsonlib--alist-keep (keys alist)
  (-filter #'(lambda (k) (memq (car k) keys)) alist))

(defun jsonlib--alist-drop (keys alist)
  (-filter #'(lambda (k) (not (memq (car k) keys))) alist))

(defun jsonlib--clean-struct (struct)
  (->> (mapcar 'car (cl-struct-slot-info struct))
       (remq '$other)
       (remq 'cl-tag-slot)
       (remq 'elisp--witness--lisp)))

(defun jsonlib--zip (list1 list2)
  (cl-loop for x in list1 for y in list2 append (list x y)))

(defmacro define-jsonlib-unmarshaller (struct)
  (if-let* ((targets (jsonlib--clean-struct struct))
            (fields (mapcar
                     #'(lambda (sym)
                         (intern (format
                                  ":%s" (car sym))))
                     (cdr
                      (cl-struct-slot-info struct)))))
      `(defun
           ,(intern
             (format "unmarshal-%s" struct))
           (data)
         (,(intern
            (format "make-%s" struct))
           ,@(jsonlib--zip fields
                           (mapcar
                            #'(lambda (sym)
                                `(alist-get (quote ,sym) data))
                            targets))
           :$other (jsonlib--alist-drop (quote ,targets) data)
           ))))



(defmacro jsonlib-unmarshal (struct data)
  (if-let* ((targets (jsonlib--clean-struct struct))
            (fields (mapcar
                     #'(lambda (sym)
                         (intern (format
                                  ":%s" (car sym))))
                     (cdr
                      (cl-struct-slot-info struct)))))
      `(,(intern
          (format "make-%s" struct))
         ,@(jsonlib--zip fields
                         (mapcar
                          #'(lambda (sym)
                              `(alist-get (quote ,sym) ,data))
                          targets))
         :$other (jsonlib--alist-drop (quote ,targets) ,data)
         )))


(cl-defstruct jsonlib-schema-object
  "A struct representing a JSON schema"
  $schema $id title description type required properties $other)

(define-jsonlib-unmarshaller jsonlib-schema-object)

(cl-defstruct jsonlib-schema-array
  "A struct representing a JSON array"
  $schema $id title description type items $other)

(define-jsonlib-unmarshaller jsonlib-schema-array)

(defun jsonlib-parse-schema (node)
  (let-alist node
    (cl-case (intern .type)
      (object (unmarshal-jsonlib-schema-object node))
      (array (unmarshal-jsonlib-schema-array node)))))

(cl-defstruct jsonlib-schema-catalog
  schema-url version catalog)

(defun jsonlib--parse-schema-catalog (object)
  (make-jsonlib-schema-catalog
   :schema-url (gethash "$schema" object)
   :version (gethash "version" object)
   :catalog (gethash "schemas" object)))

(provide 'jsonlib)
