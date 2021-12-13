;;; -*- lexical-binding: t; -*-

(require 'bindat)

(defun etf-convert-float-bytes (b0 b1 b2 b3 b4 b5 b6 b7)
  (let* ((negp (= #x80 (logand b0 #x80)))
         (exp (logand (logior (ash b0 4) (ash b1 -4)) #x7ff))
         (mantissa (logior #x10000000000000
                           (ash (logand #xf b1) 48)
                           (ash b2 40)
                           (ash b3 32)
                           (ash b4 24)
                           (ash b5 16)
                           (ash b6  8)
                           b7))
         (result (if (= #x7ff exp)
                     (if (= #x10000000000000 mantissa)
                         1.0e+INF
                       0.0e+NaN)
                   (ldexp (ldexp mantissa -53) (- exp 1022)))))
    (if negp
        (- result)
      result)))

(defun etf-convert-float-string (str)
  (string-to-number str))

(defun etf-convert-vector-int (vec)
  (cl-loop for byte across vec
        for shift downfrom 24 by 8
        for ctr = (logior 0 (ash byte shift))
        then (logior ctr (ash byte shift))
        finally (return (if (etf-int-is-negative ctr)
                            (logxor (ash -1 32) ctr)
                          ctr))))

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
    (content str (length))))

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

(provide 'etf)

