;;; Minimal utf-8 decoding and encoding library.
;;;
;;; See http://common-lisp.net/project/trivial-utf-8/

;;; This is is basically MGL-PAX:DEFINE-PACKAGE, but we don't want to
;;; depend on it. The package variance stuff is because we export
;;; documentation from the TRIVIAL-UTF-8/DOC system.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (locally
      (declare #+sbcl
               (sb-ext:muffle-conditions sb-int:package-at-variance))
    (handler-bind
        (#+sbcl (sb-int:package-at-variance #'muffle-warning))
      (defpackage :trivial-utf-8
        (:use :common-lisp)
        (:export #:utf-8-byte-length
                 #:string-to-utf-8-bytes
                 #:write-utf-8-bytes
                 #:utf-8-group-size
                 #:utf-8-bytes-to-string
                 #:read-utf-8-string
                 #:utf-8-decoding-error)))))

(in-package :trivial-utf-8)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *optimize*
    '(optimize (speed 3) (safety 0) (space 0) (debug 1)
      (compilation-speed 0))))

(defun utf-8-byte-length (string)
  "Calculate the amount of bytes needed to encode STRING."
  (declare (type string string)
           #.*optimize*)
  (let* ((string (coerce string 'simple-string))
         (length (length string)))
    (declare (type fixnum length))
    (loop :for char :across string
          :do (let ((code (char-code char)))
                (when (> code 127)
                  (incf length
                        (cond ((< code 2048) 1)
                              ((< code 65536) 2)
                              (t 3))))))
    length))

(defmacro as-utf-8-bytes (char writer)
  "Given the character CHAR, call the WRITER for every byte in the
  UTF-8 encoded form of that character. WRITER may a function or a
  macro."
  (let ((char-code (gensym)))
    `(let ((,char-code (char-code ,char)))
       (declare (type fixnum ,char-code))
       (cond ((< ,char-code 128)
              (,writer ,char-code))
             ((< ,char-code 2048)
              (,writer (logior #b11000000 (ldb (byte 5 6) ,char-code)))
              (,writer (logior #b10000000 (ldb (byte 6 0) ,char-code))))
             ((< ,char-code 65536)
              (,writer (logior #b11100000 (ldb (byte 4 12) ,char-code)))
              (,writer (logior #b10000000 (ldb (byte 6 6) ,char-code)))
              (,writer (logior #b10000000 (ldb (byte 6 0) ,char-code))))
             (t
              (,writer (logior #b11110000 (ldb (byte 3 18) ,char-code)))
              (,writer (logior #b10000000 (ldb (byte 6 12) ,char-code)))
              (,writer (logior #b10000000 (ldb (byte 6 6) ,char-code)))
              (,writer (logior #b10000000 (ldb (byte 6 0) ,char-code))))))))

(defun string-to-utf-8-bytes (string &key null-terminate)
  "Convert STRING into an array of unsigned bytes containing its UTF-8
  representation. If NULL-TERMINATE, add an extra 0 byte at the end."
  (declare (type string string)
           #.*optimize*)
  (let* ((string (coerce string 'simple-string))
         (buffer (make-array (+ (the fixnum (utf-8-byte-length string))
                                (if null-terminate 1 0))
                             :element-type '(unsigned-byte 8)))
         (position 0))
    (declare (type (array (unsigned-byte 8)) buffer)
             (type fixnum position))
    (macrolet ((add-byte (byte)
                 `(progn (setf (aref buffer position) ,byte)
                         (incf position))))
      (loop :for char :across string
            :do (as-utf-8-bytes char add-byte)))
    (when null-terminate
      (setf (elt buffer (1- (length buffer))) 0))
    buffer))

(defun write-utf-8-bytes (string byte-stream &key null-terminate)
  "Write STRING to BYTE-STREAM, encoding it as UTF-8. If
  NULL-TERMINATE, write an extra 0 byte at the end."
  (declare (type string string)
           (type stream byte-stream)
           #.*optimize*)
  (macrolet ((byte-out (byte)
               `(write-byte ,byte byte-stream)))
    (let ((string (coerce string 'simple-string)))
      (loop :for char :across string
            :do (as-utf-8-bytes char byte-out))))
  (when null-terminate
    (write-byte 0 byte-stream)))

(define-condition utf-8-decoding-error (simple-error)
  ((message :initarg :message)
   (byte :initarg :byte :initform nil))
  (:report (lambda (err stream)
             (format stream (slot-value err 'message)
                     (slot-value err 'byte)))))

(declaim (inline utf-8-group-size))
(defun utf-8-group-size (byte)
  "Determine the amount of bytes that are part of the character whose
  encoding starts with BYTE. May signal UTF-8-DECODING-ERROR."
  (declare (type fixnum byte)
           #.*optimize*)
  (cond ((not (logtest byte #b10000000)) 1)
        ((= (logand byte #b11100000) #b11000000) 2)
        ((= (logand byte #b11110000) #b11100000) 3)
        ((= (logand byte #b11111000) #b11110000) 4)
        (t (error 'utf-8-decoding-error :byte byte
                  :message "Invalid byte at start of character: 0x~X"))))

(declaim (inline utf-8-string-length))
(defun utf-8-string-length (bytes &key (start 0) (end (length bytes)))
  "Calculate the length of the string encoded by the subsequence of
  the array of BYTES bounded by START and END."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes)
           (type fixnum start end)
           #.*optimize*)
  (loop :with i :of-type fixnum = start
        :with string-length :of-type fixnum = 0
        :while (< i end)
        :do (progn
              (incf (the fixnum string-length) 1)
              (incf i (utf-8-group-size (elt bytes i))))
        :finally (return string-length)))

(declaim (inline get-utf-8-character))
(defun get-utf-8-character (bytes group-size &optional (start 0))
  "Extract the character from an array of BYTES encoded in GROUP-SIZE
  number of bytes from the START position. May signal
  UTF-8-DECODING-ERROR."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes)
           (type fixnum group-size start)
           #.*optimize*)
  (macrolet ((next-byte ()
               '(prog1 (elt bytes start)
                 (incf start)))
             (six-bits (byte)
               (let ((b (gensym)))
                 `(let ((,b ,byte))
                    (unless (= (logand ,b #b11000000) #b10000000)
                      (error 'utf-8-decoding-error :byte ,b
                             :message "Invalid byte 0x~X inside a character."))
                    (ldb (byte 6 0) ,b))))
             (test-overlong (byte min-size)
               (let ((b (gensym)))
                 `(let ((,b ,byte))
                    (unless (>= ,b ,min-size)
                      (error 'utf-8-decoding-error :byte ,b
                             :message "Overlong byte sequence found."))
                    ,b))))
    (case group-size
      (1 (next-byte))
      (2 (test-overlong (logior (ash (ldb (byte 5 0) (next-byte)) 6)
                                (six-bits (next-byte))) 128))
      (3 (test-overlong (logior (ash (ldb (byte 4 0) (next-byte)) 12)
                                (ash (six-bits (next-byte)) 6)
                                (six-bits (next-byte))) 2048))
      (4 (test-overlong (logior (ash (ldb (byte 3 0) (next-byte)) 18)
                                (ash (six-bits (next-byte)) 12)
                                (ash (six-bits (next-byte)) 6)
                                (six-bits (next-byte))) 65536)))))

(defun utf-8-bytes-to-string (bytes &key (start 0) (end (length bytes)))
  "Convert the START, END subsequence of the array of BYTES containing
  UTF-8 encoded characters to a STRING. The element type of BYTES may
  be anything as long as it can be `COERCE`d into an `(UNSIGNED-BYTES
  8)` array. May signal UTF-8-DECODING-ERROR."
  (declare (type vector bytes)
           (type fixnum start end)
           #.*optimize*)
  (loop :with bytes = (coerce bytes '(simple-array (unsigned-byte 8) (*)))
        :with buffer = (make-string (utf-8-string-length bytes :start start
                                                         :end end)
                                    :element-type 'character)
        :with array-position :of-type fixnum = start
        :with string-position :of-type fixnum = 0
        :while (< array-position end)
        :do (let* ((char (elt bytes array-position))
                   (current-group (utf-8-group-size char)))
              (when (> (+ current-group array-position) end)
                (error 'utf-8-decoding-error
                       :message "Unfinished character at end of byte array."))
              (setf (char buffer string-position)
                    (code-char (get-utf-8-character bytes current-group
                                                    array-position)))
              (incf string-position 1)
              (incf array-position current-group))
        :finally (return buffer)))

(defun read-utf-8-string (input &key null-terminated stop-at-eof
                          (char-length -1) (byte-length -1))
  "Read UTF-8 encoded data from INPUT, a byte stream, and construct a
  string with the characters found. When NULL-TERMINATED is given,
  stop reading at a null character. If STOP-AT-EOF, then stop at
  END-OF-FILE without raising an error. The CHAR-LENGTH and
  BYTE-LENGTH parameters can be used to specify the max amount of
  characters or bytes to read, where -1 means no limit. May signal
  UTF-8-DECODING-ERROR."
  (declare (type stream input)
           (type fixnum byte-length char-length)
           #.*optimize*)
  (let ((buffer (make-array 4 :element-type '(unsigned-byte 8)))
        (bytes-read 0)
        (string (make-array 64 :element-type 'character
                            :adjustable t :fill-pointer 0)))
    (declare (type fixnum bytes-read))
    (loop
      (when (or (and (/= -1 byte-length) (>= bytes-read byte-length))
		(and (/= -1 char-length) (= char-length (length string))))
	(return))
      (let ((next-char (read-byte input (not stop-at-eof) :eof)))
	(when (or (eq next-char :eof)
		  (and null-terminated (eq next-char 0)))
	  (return))
	(let ((current-group (utf-8-group-size next-char)))
	  (incf bytes-read current-group)
	  (cond ((= current-group 1)
		 (vector-push-extend (code-char next-char) string))
		(t
		 (setf (elt buffer 0) next-char)
		 (loop :for i :from 1 :below current-group
		       :for next-char = (read-byte input nil :eof)
		       :do (when (eq next-char :eof)
			     (error 'utf-8-decoding-error
				    :message
                                    "Unfinished character at end of input."))
		       :do (setf (elt buffer i) next-char))
		 (vector-push-extend (code-char (get-utf-8-character
						 buffer current-group))
				     string))))))
    string))
