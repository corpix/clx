(in-package #:cl-user)
(defpackage clx/binary
  (:use #:clx/std)
  (:local-nicknames (#:lb #:lisp-binary))
  (:export #:read-integer
	   #:read-integer-be
	   #:read-integer-le
	   #:write-integer
	   #:write-integer-be
	   #:write-integer-le))
(in-package #:clx/binary)

(reexport-from :cl-octet-streams :include
	       '(:make-octet-input-stream
		 :make-octet-output-stream))
(reexport-from :babel :include
	       '(:string-to-octets
		 :octets-to-string))
(reexport-from :lisp-binary :include
	       '(:read-bytes
		 :write-bytes))
(reexport-from :cl-intbytes :exclude
	       '(:octets->int
		 :int->octets))
(reexport-from :bit-smasher)

;;

(defmacro read-integer (size stream byte-order &rest rest)
  "Wrapper around LISP-BINARY:READ-INTEGER ensuring the byte order, passing through REST parameters to LISP-BINARY:READ-INTEGER"
  `(lb:read-integer ,size ,stream :byte-order ,byte-order ,@rest))

(defmacro write-integer (value size stream byte-order &rest rest)
  "Wrapper around LISP-BINARY:READ-INTEGER ensuring the byte order, passing through REST parameters to LISP-BINARY:WRITE-INTEGER"
  `(lb:write-integer ,value ,size ,stream :byte-order ,byte-order ,@rest))

(defmacro read-integer-be (size stream &rest rest)
  "Read integer of SIZE from STREAM in Big-Endian byte order"
  `(read-integer ,size ,stream :big-endian ,@rest))

(defmacro read-integer-le (size stream &rest rest)
  "Read integer of SIZE from STREAM in Little-Endian byte order"
  `(read-integer ,size ,stream :little-endian ,@rest))

(defmacro write-integer-be (value size stream &rest rest)
  "Write integer VALUE of SIZE to STREAM in Big-Endian byte order"
  `(write-integer ,value ,size ,stream :big-endian ,@rest))

(defmacro write-integer-le (value size stream &rest rest)
  "Write integer VALUE of SIZE to STREAM in Little-Endian byte order"
  `(write-integer ,value ,size ,stream :little-endian ,@rest))
