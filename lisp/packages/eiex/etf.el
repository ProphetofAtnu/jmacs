;;; -*- lexical-binding: t; -*-

(require 'bindat)

;; Bits/Bytes
(defun etf--byte-to-bits (int)
  (cl-loop for x downfrom 7 to 0
        collect
        (logand 1 (ash int (- x)))))

(defun etf--bytes-to-bits (bytes)
  (seq-mapcat 'etf--byte-to-bits bytes))

(defun etf--bits-to-uint (bits)
  (cl-loop for x in bits
        for i downfrom (1- (length bits))
        with num = 0
        do (setq num
                 (logior num
                         (ash x i)))
        finally (return num)))

(defsubst etf--8bit-to-byte (n7 n6 n5 n4 n3 n2 n1 n0)
  (logior
   (ash n7 7)
   (ash n6 6)
   (ash n5 5)
   (ash n4 4)
   (ash n3 3)
   (ash n2 2)
   (ash n1 1)
   n0))

(defun etf--bits-to-bytes (bits)
  (seq-map
   #'(lambda (x)
       (apply 'etf--8bit-to-byte x))
   (seq-partition bits 8)))

(defun etf--uint-to-bits (i len)
  (cl-loop for s downfrom (- len 1) to 0
        collect (logand 1 (ash i (- s)))))

;; Floats
(defun etf--float-exp-bits (bits)
  (- (etf--bits-to-uint (seq-subseq bits 1 12))
     1023))

(defun etf--float-sig-bits (bits)
  (cl-loop for b in (seq-subseq bits 12)
        for i from 1
        with frac = 0
        do (when (> b 0)
             (setq frac (+ frac (expt 2 (- i)))))
        finally (return (+ 1 frac))))

(defun etf--float-from-bits (bits)
  (*
   (ldexp 
    (etf--float-sig-bits bits)
    (etf--float-exp-bits bits))
   (expt -1 (seq-first bits))))

(defun etf--float-from-bits-DEBUG (bits)
  (cons (etf--float-sig-bits bits)
        (etf--float-exp-bits bits)))

(defun etf--float-to-bits (flt)
  (let* ((e (logb flt))
         (s (- flt (expt 2 e))))
      (let ((sign (if (>= s 0) 0 1))
            (expb (etf--uint-to-bits (+ e 1023) 11))
            (sigb (cl-loop for i from 1 to 52
                        for h = (* (math-abs s) 2) then (* h 2)
                        if (>= h 1)
                        collect 1
                        and do (cl-decf h)
                        else collect 0)))
        (append (cons sign expb) sigb))))

(defun etf--float-from-bytes (vec)
  (etf--float-from-bits (etf--bytes-to-bits vec)))

(defun etf--float-to-bytes (flt)
  (apply
   'vector
   (etf--bits-to-bytes
    (etf--float-to-bits flt))))

(defun etf--convert-float-string (str)
  (string-to-number (string-trim str)))

;; Integers
(defun etf--int-is-negative (int)
  (= #x80000000
     (logand #x80000000 int)))

(defun etf--convert-vector-int (vec)
  (cl-loop for byte across vec
        for shift downfrom (* 8 (1- (length vec))) by 8
        for ctr = (logior 0 (ash byte shift))
        then (logior ctr (ash byte shift))
        finally (return (if (etf--int-is-negative ctr)
                            (logxor (ash -1 32) ctr)
                          ctr))))

(defun etf--convert-int-vector (n)
  (let ((uns (logand #xFFFFFFFF n)))
    (vector
     (logand (ash uns -24) #xFF)
     (logand (ash uns -16) #xFF)
     (logand (ash uns -8) #xFF)
     (logand uns #xFF))))

;; Bindat structures
(defvar etf-integer
  '((integer vec 4)))

(defvar etf-small-integer
  '((integer vec 1)))

(defvar etf-atom-small
  '((length u8)
    (name str (length))))

(defvar etf-atom
  '((length u16)
    (name str (length))))

(defvar etf-float
  '((float str 31)))

(defvar etf-new-float
  '((float vec 8)))

(defvar etf-atom-cache-ref
  '((cache-index u8)))

(defvar etf-bit-binary
  '((length u32)
    (bits u8)
    (data vec (length))))

(defvar etf-pid
  '((node struct etf-data)
    (id u32)
    (serial u32)
    (creation 8)))

(defvar etf-new-pid
  '((node struct etf-data)
    (id u32)
    (serial u32)
    (creation u32)))

(defvar etf-v4-port
  '((node struct etf-data)
    (id vec 8)
    (creation u32)))

(defvar etf-port 
  '((node struct etf-data)
    (id u32)
    (creation u8)))

(defvar etf-new-port 
  '((node struct etf-data)
    (id u32)
    (creation u32)))

(defvar etf-newer-reference
  '((length u16)
    (node struct etf-data)
    (creation u32)
    (ids vec length u32)))

(defvar etf-new-reference
  '((length u16)
    (node struct etf-data)
    (creation u8)
    (ids vec length u32)))

(defvar etf-reference
  '((length u16)
    (node struct etf-data)
    (id u32)
    (creation u8)))

(defvar etf-small-tuple
  '((arity u8)
    (elements repeat (arity)
     (struct etf-data))))

(defvar etf-large-tuple 
  '((arity u32)
    (elements repeat (arity)
     (struct etf-data))))

(defvar etf-string
  '((length u16)
    (data str (length))))

(defvar etf-list 
  '((length u32)
    (elements repeat (length)
     (struct etf-data))
    (tail (struct etf-data))))

(defvar etf-map-pair '((key struct etf-data)
                       (val struct etf-data)))

(defvar etf-map '((arity u32)
                  (pairs repeat (arity)
                   (struct etf-map-pair))))

(defvar etf-binary
  '((length u32)
    (data vec (length))))

(defvar etf-small-big
  '((length u8)
    (sign u8)
    (data vec (length))))

(defvar etf-large-big 
  '((length u32)
    (sign u8)
    (data vec (length))))

;; TODO
(defvar etf-export '())
(defvar etf-new-fun '())
(defvar etf-fun '())


(defvar etf-data
  '((type u8)
    (union (type)
     (70 (struct etf-new-float))
     (77 (struct etf-bit-binary))
     (82 (struct etf-atom-cache-ref))
     (88 (struct etf-new-pid))
     (89 (struct etf-new-port))
     (90 (struct etf-newer-reference))
     (97 (struct etf-small-integer))
     (98 (struct etf-integer))
     (99 (struct etf-float))
     (100 (struct etf-atom))
     (101 (struct etf-reference))
     (102 (struct etf-port))
     (103 (struct etf-pid))
     (104 (struct etf-small-tuple))
     (105 (struct etf-large-tuple))
     (106 (struct etf-nil))
     (107 (struct etf-string))
     (108 (struct etf-list))
     (109 (struct etf-binary))
     (110 (struct etf-small-big))
     (111 (struct etf-large-big))
     (112 (struct etf-new-fun))
     (113 (struct etf-export))
     (114 (struct etf-new-reference))
     (115 (struct etf-atom-small))
     (116 (struct etf-map))
     (117 (struct etf-fun))
     (118 (struct etf-atom))
     (119 (struct etf-atom-small))
     (120 (struct etf-v4-port)))))

(defvar etf-packet
  `((version u8)
    (struct etf-data)))

(defun etf-unpack (data)
  (bindat-unpack etf-packet data))

(defun etf-struct-type (struct)
  (let ((code (alist-get 'type struct)))
    (alist-get code etf-data-types-alist)))

(defun etf--convert-bin (struct)
  "Struct converter for etf-string and etf-binary"
  (let-alist struct
    (concat .data)))

(defun etf--revert-bin (arg)
  `((type 109)
    (data . (string-to-vector arg))
    (length . (string-bytes arg))))

(defun etf--convert-int (struct)
  (etf--convert-vector-int (alist-get 'integer struct)))

(defun etf--revert-int (int)
  `((type . 98)
    (integer . ,(etf--convert-int-vector int))))

(defun etf--convert-float (struct)
  (let ((flv (alist-get 'float struct)))
    (if (vectorp flv)
        (etf--float-from-bytes flv)
      (etf--convert-float-string flv))))

(defun etf--revert-float (flt)
  (list (cons 'type 70)
        (cons 'float (etf--float-to-bytes flt))))

(defun etf--convert-atom (struct)
  (intern (alist-get 'name struct)))

(defun etf--revert-atom (arg)
  (let* ((n (symbol-name arg))
         (l (string-bytes n)))
    `((type . 100)
      (length . ,l)
      (name . ,n))))

(defun etf--convert-bigint (struct)
  (* (expt -1 (alist-get 'sign struct))
     (cl-loop for x across  (alist-get 'data struct)
           for y from 0
           sum (* x (expt 256 y)))))

(defun etf--revert-bigint (arg)
  (cl-loop 
      for offset from 0 by 8 
      for x = (ash (abs arg) (- offset))
      while (> x 0)
      collect (logand x #xFF) into bin
      finally (return
                `((type . 111)
                  (length . ,(length bin))
                  (sign . ,(if (< arg 0)
                               1 0))
                  (data . ,(vconcat bin))))))

(defun etf--convert-list (arg)
  (mapcar #'etf-convert (alist-get 'elements arg)))

(defun etf--revert-list (arg)
  `((type . 108)
    (length . ,(length arg))
    (elements . ,(mapcar #'etf-revert arg))))

(defun etf--convert-tuple (arg)
  (cl-map 'vector #'etf-convert (alist-get 'elements arg)))

(defun etf--revert-tuple (arg)
  `((type . 104)
    (arity . ,(length arg))
    (elements . ,(mapcar #'etf-revert arg))))

(defun etf--convert-map (arg)
  (let ((hm (make-hash-table)))
    (dolist (s (alist-get 'pairs arg) hm)
      (let ((hk (alist-get 'key s))
            (hv (alist-get 'val s)))
        (puthash (etf-convert hk) (etf-convert hv) hm)))))

(defun etf--revert-map-pair (key val)
  `((key . ,(etf-revert key))
    (val . ,(etf-revert val))))

(defun etf--revert-map (arg)
  (let ((len (hash-table-size))))
  `((type . 116)
    (arity . ,(hash-table-count arg))
    (pairs . ,(cl-loop for k being the hash-keys of arg
                    using hash-values v
                    collect (etf--revert-pair k v)))))

(defvar etf-data-types-alist
  '((70 . new-float)
    (77  . bit-binary)
    (82  . atom-cache-ref)
    (88  . new-pid)
    (89  . new-port)
    (90  . newer-reference)
    (97  . small-integer)
    (98  . integer)
    (99  . float)
    (100  . atom)
    (101  . reference)
    (102  . port)
    (103  . pid)
    (104  . small-tuple)
    (105  . large-tuple)
    (106  . nil)
    (107  . string)
    (108  . list)
    (109  . binary)
    (110  . small-big)
    (111  . large-big)
    (112  . new-fun)
    (113  . export)
    (114  . new-reference)
    (115  . atom-small)
    (116  . map)
    (117  . fun)
    (118  . atom)
    (119  . atom-small)
    (120  . v4-port)))

(defun etf-convert (struct)
  (cl-case (etf-struct-type struct)
    ('nil nil)
    ('new-float (etf--convert-float struct))
    ('float (etf--convert-float-string struct))
    ('atom (etf--convert-atom struct))
    ('atom-small (etf--convert-atom struct))
    ('integer (etf--convert-int struct))
    ('small-integer (etf--convert-int struct))
    ('binary (etf--convert-bin struct))
    ('string (etf--convert-bin struct))
    ('small-big (etf--convert-bigint struct))
    ('large-big (etf--convert-bigint struct))
    ('small-tuple (etf--convert-tuple struct))
    ('large-tuple (etf--convert-tuple struct))
    ('list (etf--convert-list struct))
    ('map (etf--convert-map struct))
    (t struct)))

(defun etf-revert (term)
  (cond
    ((integerp term) (etf--revert-int term))
    ((bignump term) (etf--revert-bigint term))
    ((floatp term) (etf--revert-float term))
    ((symbolp term) (etf--revert-atom term))
    ((stringp term) (etf--revert-bin term))
    ((listp term) (etf--revert-list term))
    ((vectorp term) (etf--revert-tuple term))
    ((hash-table-p term) (etf--revert-map term))))

(provide 'etf)

