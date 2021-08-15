;;;; core.lisp

;;;; Copyright (c) 2014--2015, "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :bit-smasher)

(declaim (type (simple-array (simple-bit-vector 4) (16)) *bit-map*))
(defvar *bit-map* #(#*0000
                    #*0001
                    #*0010
                    #*0011
                    #*0100
                    #*0101
                    #*0110
                    #*0111
                    #*1000
                    #*1001
                    #*1010
                    #*1011
                    #*1100
                    #*1101
                    #*1110
                    #*1111))

(deftype hex-char ()
  `(member #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
           #\a #\b #\c #\d #\e #\f
           #\A #\B #\C #\D #\E #\F))

(declaim (ftype (function (hex-char) (integer 0 16)) hexchar->int)
         (inline hexchar->int))
(defun hexchar->int (char)
  "Return the bit vector associated with a hex-value character CHAR from *bit-map*."
  (declare (optimize (speed 2) (safety 0)))
  (cond ((char<= #\0 char #\9) (- (char-code char) #.(char-code #\0)))
        ((char<= #\a char #\f) (- (char-code char) #.(- (char-code #\a) 10)))
        (t                     (- (char-code char) #.(- (char-code #\A) 10))
         ;; always return these results
         #+nil (char<= #\A char #\F))))

(declaim (ftype (function (hex-char) (simple-bit-vector 4)) hex-to-bit-lookup/unsafe))
(defun hex-to-bit-lookup/unsafe (char)
  "Return the bit vector associated with a hex-value character CHAR from *bit-map*."
  (declare (optimize (speed 2) (safety 0)))
  (aref *bit-map* (hexchar->int char)))

(defun hex-to-bit-lookup (char)
  "Return the bit vector associated with a hex-value character CHAR from *bit-map*."
  (declare (optimize (speed 2) (safety 0)))
  (copy-seq (hex-to-bit-lookup/unsafe char)))

