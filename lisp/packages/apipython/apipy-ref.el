;; -*- lexical-binding: t; -*-

(require 'eieio)
(require 'apipy)

(defclass apipy-ref ()
    ((proc :initarg :proc)
     (ref :initarg :ref)))

(slot-value (apipy-ref :proc 1 :ref 1) :proc)

(cl-defgeneric apy-live-p (ref)
  "Check if the reference has a valid process")

(cl-defmethod apy-live-p ((ref apipy-ref))
  (process-live-p (slot-value ref :proc)))

(cl-defgeneric apy-call (refr method &key args kwargs transform))

(cl-defmethod apy-call ((refr apipy-ref) method &key args kwargs transform)
  (when (apy-live-p refr)
    (with-slots (proc ref) refr
        (apipy-send-thunk proc method :target ref
                          :args args
                          :kwargs kwargs
                          :transform transform))))

(cl-defgeneric apy-call-async (refr cb method &key args kwargs))

(cl-defmethod apy-call-async ((refr apipy-ref) cb method &key args kwargs transform)
  (when (apy-live-p refr)
    (with-slots (proc ref) refr
      (apipy-send-async
       proc cb method
       :target ref
       :args args
       :kwargs kwargs
       :transform transform))))

(cl-defmethod apy-describe ((refr apipy-ref) &key (full? nil))
  (when (apy-live-p refr)
     (with-slots (proc ref) refr
       (apipy-send-thunk
        proc
        "describe"
        :args (list ref)
        :transform
        (lambda (res)
          (if full?
              res
            (cl-loop for (k . v) in res
                  collect (cons k (if (stringp v)
                                      v
                                      (seq-remove
                                       (apply-partially #'string-prefix-p
                                                        "_")
                                       v)))))
          )))))

(provide 'apipy-ref)
