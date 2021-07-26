(in-package :cl-user)
(defpackage clx/nsq
  (:use :cl :alexandria :iterate :usocket :babel :lisp-binary))
(in-package :clx/nsq)

;; see: https://nsq.io/clients/tcp_protocol_spec.html

(defconstant +newline+ (char-code #\newline)
  "Newline character code")

(defconstant +whitespace+ (char-code #\space)
  "Whitespace character code")

(defconstant +magic+
  (string-to-octets "  V2")
  "Protcol MAGIC string which is used during handshake")

(defconstant +size+ 4
  "Frame SIZE in bytes")

;;

(defconstant +frame-type-response+ 0
  "Successful result for a sent frame or a heartbeat frame")
(defconstant +frame-type-error+ 1
  "Failure result for a sent frame")
(defconstant +frame-type-message+ 2
  "Message payload result for a sent frame")

(defconstant +frame-payload-ok+
  (string-to-octets "OK")
  "Frame payload representing successful condition")
(defconstant +frame-payload-heartbeat+
  (string-to-octets "_heartbeat_")
  "Frame payload representing heartbeat")

;;

(defparameter *connection* nil
  "Nsq client connection socket")

;;

(define-condition frame-error (error)
  ((inner :reader frame-error-inner :initarg :inner))
  (:documentation "Error returned by the NSQ protocol as a response to some protocol frame")
  (:report (lambda (condition stream)
	     (format stream "NSQ responded with error ~s"
		     (frame-error-inner condition)))))

(define-condition unsupported-frame-type-error (error)
  ((subject :reader unsupported-frame-type-error-subject :initarg :subject))
  (:documentation "Error returned by the protocol handlers to signal an unsupported frame type received")
  (:report (lambda (condition stream)
	     (format stream "Unsupported frame type received ~s"
		     (unsupported-frame-type-error-subject condition)))))

;;

(defun connect (&key (host "127.0.0.1") (port 4150))
  "Connect to the NSQ daemon using optional HOST and PORT keywords"
  (let* ((connection (socket-connect host port :element-type '(unsigned-byte 8)))
	 (stream (socket-stream connection)))
    (handler-case (progn (write-sequence +magic+ stream)
			 (force-output stream))
      (error (c)
	(socket-close connection)
	(error c)))
    connection))

;;

(defun write-frame-command (command stream &key (parameters nil))
  "Write COMMAND as part of a frame to the STREAM. Optional PARAMETERS may be set in the format '(:key1 value1 :key2 value2 ...)"
  (write-sequence (string-to-octets
		   (cond ((or (keywordp command)
			      (symbolp command))
			  (string command))
			 (t command)))
		  stream)
  (when parameters
    (iter (for parameter in parameters)
      (write-byte +whitespace+ stream)
      (write-sequence
       (cond ((stringp parameter) (string-to-octets parameter))
	     ((or (keywordp parameter)
		  (symbolp parameter))
	      (string-to-octets (string parameter)))
	     (t parameter))
       stream))))

(defun write-frame-size (payload stream)
  "Write size of a frame PAYLOAD to the STREAM as a Big Endian +SIZE+ integer"
  (write-integer (length payload) +size+ stream
		 :byte-order :big-endian))

(defun write-frame-payload (payload stream)
  "Write PAYLOAD of a frame into the STREAM"
  (let ((buffer (cond ((stringp payload) (string-to-octets payload))
		      (t payload))))
    (write-frame-size buffer stream)
    (write-sequence buffer stream)))

(defun write-frame (command payload stream &key (parameters nil))
  "Write a frame COMMAND and PAYLOAD into the STREAM. Optional PARAMETERS may be set in the format '(:key1 value1 :key2 value2 ...)"
  (write-frame-command command stream :parameters parameters)
  (write-byte +newline+ stream)
  (when payload
    (write-frame-payload payload stream))
  (force-output stream))

;;

(defun read-frame (stream)
  "Read frame from STREAM and return T | MESSAGE or signal a condition FRAME-ERROR | UNSUPPORTED-FRAME-TYPE-ERROR"
  (let* ((size (- (read-integer +size+ stream :byte-order :big-endian) +size+))
	 (frame-type (read-integer +size+ stream :byte-order :big-endian))
	 (buffer (read-bytes size stream)))
    (cond ((= frame-type +frame-type-response+)
	   (cond ((equalp buffer +frame-payload-ok+) 'ok)
		 ((equalp buffer +frame-payload-heartbeat+) 'heartbeat)))
	  ((= frame-type +frame-type-error+) (error 'frame-error :inner (octets-to-string buffer)))
	  ((= frame-type +frame-type-message+) 'ah-we-got-a-message-this-is-not-implemented-yet)
	  (t (error 'unsupported-frame-type-error :subject frame-type)))))

;;

(defun identify (&key (parameters :empty) (connection *connection*))
  "Send IDENTIFY command into *CONNECTION*. Optional PARAMETERS may be set in the format '(:key1 value1 :key2 value2 ...)"
  (write-frame
   :identify (jojo:to-json parameters :octets t)
   (socket-stream connection)))

(defun nop (&key (connection *connection*))
  "Send NOP command into *CONNECTION*"
  (write-frame :nop nil (socket-stream connection)))

(defun publish (topic payload &key (connection *connection*))
  "Send PUBLISH command with TOPIC and PAYLOAD into *CONNECTION*"
  (let ((stream (socket-stream connection)))
    (write-frame :pub payload stream
		 :parameters (list topic))
    (read-frame stream)))

;;

(defmacro with-connect (parameters &body body)
  "Connect to NSQ with PARAMETERS and execute BODY, closing the socket after execution"
  `(let ((*connection* (connect ,@parameters)))
     (unwind-protect (progn ,@body)
       (and *connection* (socket-close *connection*)))))

;; FIXME: how to make iter clauses inside BODY (providen by user) more explicit/confined?
(defmacro with-iter (var &body body)
  "Iterate *CONNECTION* stream reading frame into VAR and executing BODY for each frame"
  (with-gensyms (stream)
    `(let ((,stream (socket-stream *connection*)))
       (iter (for ,var next (read-frame ,stream))
	 (cond ((equalp ,var 'heartbeat)
		(nop)
		(next-iteration))
	       (t ,@body))))))

(let ((n 0))
  (with-connect (:port 4150)
    (identify :parameters '(:heartbeat_interval 1000))
    (with-iter frame
      (while (< n 5))
      (format t "~a~C" frame #\newline)
      (setf n (+ 1 n)))))
