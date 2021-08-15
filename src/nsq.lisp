(in-package #:cl-user)
(defpackage clx/nsq
  (:use #:clx/std #:clx/binary #:clx/channel #:clx/time)
  (:local-nicknames (:json :clx/json)
		    (:streams :clx/stream))
  (:import-from :clx/stream
		#:make-flexi-stream
		#:peek-byte
		#:*null-stream*)
  (:export #:message
	   #:message-p
	   #:message-timestamp
	   #:message-attempts
	   #:message-id
	   #:message-stream

	   #:nsq-error
	   #:nsq-unsupported-frame-type

	   #:nsq
	   #:close
	   #:publish
	   #:subscribe
	   #:ready
	   #:finish
	   #:requeue
	   #:touch
	   #:make-nsq))
(in-package #:clx/nsq)

;; see: https://nsq.io/clients/tcp_protocol_spec.html

(defconstant +newline+ (char-code #\newline)
  "Newline character code")

(defconstant +whitespace+ (char-code #\space)
  "Whitespace character code")

(defvar +magic+
  (string-to-octets "  V2")
  "Protcol MAGIC string which is used during handshake")

;;

(defconstant +frame-size+ 4
  "Frame SIZE in bytes")
(defconstant +frame-type-size+ 4
  "Frame type SIZE in bytes")

(defconstant +frame-type-response+ 0
  "Successful result for a sent frame or a heartbeat frame")
(defconstant +frame-type-error+ 1
  "Failure result for a sent frame")
(defconstant +frame-type-message+ 2
  "Message payload result for a sent frame")

(defvar +frame-payload-heartbeat+
  (string-to-octets "_heartbeat_")
  "Frame payload representing heartbeat")
(defvar +frame-payload-ok+
  (string-to-octets "OK")
  "Frame payload representing successful condition")
(defvar +frame-payload-close+
  (string-to-octets "CLOSE_WAIT")
  "Frame payload representing successful connection close")

;;

(defconstant +frame-message-timestamp-size+ 8
  "Message timestamp SIZE in bytes")
(defconstant +frame-message-attempts-size+ 2
  "Message attempts SIZE in bytes")
(defconstant +frame-message-id-size+ 16
  "Message id SIZE in bytes")

;;

(define-condition nsq-error (error)
  ((inner :reader nsq-error-inner :initarg :inner))
  (:documentation "Error returned by the NSQ protocol as a response to some protocol frame")
  (:report (lambda (condition stream)
	     (format stream "NSQ responded with error ~s"
		     (nsq-error-inner condition)))))

(define-condition nsq-unsupported-frame-type (error)
  ((subject :reader nsq-unsupported-frame-type-subject :initarg :subject))
  (:documentation "Error returned by the protocol handlers to signal an unsupported frame type received")
  (:report (lambda (condition stream)
	     (format stream "Unsupported frame type received ~s"
		     (nsq-unsupported-frame-type-subject condition)))))

;;

(defstruct frame
  "Represents a protocol FRAME"
  (size nil :type number)
  (type nil :type symbol)
  (stream nil :type stream))

(defstruct message
  "Represents a MESSAGE from TOPIC"
  (size nil :type number)
  (timestamp nil :type timestamp)
  (attempts nil :type number)
  (id nil :type number)
  (stream nil :type stream))

;;

(defun write-frame-command (command stream &key (arguments nil))
  "Write COMMAND as part of a frame to the STREAM. Optional ARGUMENTS may be set"
  (write-bytes (string-to-octets
		(cond ((or (keywordp command)
			   (symbolp command))
		       (string command))
		      (t command)))
	       stream)
  (when arguments
    (dolist (argument arguments)
      (write-byte +whitespace+ stream)
      (write-sequence
       (cond ((stringp argument) (string-to-octets argument))
	     ((numberp argument) (string-to-octets (write-to-string argument)))
	     ((or (keywordp argument)
		  (symbolp argument))
	      (string-to-octets (string argument)))
	     (t argument))
       stream))))

(defun write-frame-size (payload stream)
  "Write size of a frame PAYLOAD to the STREAM as a Big Endian +FRAME-SIZE+ integer"
  (write-integer-be (length payload) +frame-size+ stream))

(defun write-frame-payload (payload stream)
  "Write PAYLOAD of a frame into the STREAM"
  (let ((buffer (cond ((stringp payload) (string-to-octets payload))
		      (t payload))))
    (write-frame-size buffer stream)
    (write-sequence buffer stream)))

(defun write-frame (command payload stream &key (arguments nil))
  "Write a frame COMMAND and PAYLOAD into the STREAM. Optional PARAMETERS may be set in the format '(:key1 value1 :key2 value2 ...)"
  (write-frame-command command stream :arguments arguments)
  (write-byte +newline+ stream)
  (when payload
    (write-frame-payload payload stream))
  (force-output stream))

(defun read-frame (stream)
  "Read FRAME from STREAM and return"
  (let ((size (read-integer-be +frame-size+ stream))
	(type (read-integer-be +frame-type-size+ stream)))
    (make-frame :size (- size +frame-type-size+)
		:type (cond
			((= type +frame-type-message+) 'message)
			((= type +frame-type-response+) 'response)
			((= type +frame-type-error+) 'error))
		:stream (make-flexi-stream stream
					   :external-format :utf-8
					   :bound (- size +frame-type-size+)))))

;;

(defun dispatch-frame (frame)
  "Dispatch FRAME, returns MESSAGE or symbol designating the received FRAME otherwise returns and error condition"
  (with-slots (stream type) frame
    (cond
      ((eq type 'message)
       (let ((timestamp (unixnano-to-timestamp
			 (read-integer-be
			  +frame-message-timestamp-size+
			  stream)))
	     (attempts (read-integer-be +frame-message-attempts-size+ stream))
	     (id (hex->int
		  (octets-to-string
		   (read-bytes +frame-message-id-size+ stream)))))
	 (make-message :size (- (frame-size frame)
				+frame-message-timestamp-size+
				+frame-message-attempts-size+
				+frame-message-id-size+)
		       :timestamp timestamp
		       :attempts attempts
		       :id id
		       :stream stream)))
      ((eq type 'response)
       (let ((buffer (read-stream-content-into-byte-vector stream :initial-size 16)))
	 (cond ((equalp buffer +frame-payload-heartbeat+) 'heartbeat)
	       ((equalp buffer +frame-payload-ok+) 'ok)
	       ((equalp buffer +frame-payload-close+) 'close)
	       (t (error (format nil "unsupported frame payload ~a" (octets-to-string buffer)))))))
      ((eq type 'error)
       (error 'nsq-error :inner
	      (octets-to-string
	       (read-stream-content-into-byte-vector
		stream
		:initial-size 64))))
      (t (error 'nsq-unsupported-frame-type :subject type)))))

;;

(defun connect (&key (host "127.0.0.1") (port 4150) (timeout 30))
  "Connect to the NSQ daemon using optional HOST and PORT keywords"
  (let* ((connection (socket-connect host port
				     :element-type '(unsigned-byte 8)
				     :timeout timeout))
	 (stream (socket-stream connection)))
    (handler-case (progn (write-sequence +magic+ stream)
			 (force-output stream))
      (error (c)
	(socket-close connection)
	(error c)))
    connection))

;;

(defclass nsq ()
  ((host :initarg :host :type string)
   (port :initarg :port :type integer)
   (connect-timeout :initarg :connect-timeout :initform 30 :type real)
   (parameters :initarg :parameters :initform :empty :type (or keyword list))
   (connection :type stream-usocket)
   (mode :initform nil :type (or keyword null))
   (lock :type lock)
   (response-queue :type bounded-channel)
   (worker :type thread)
   (handler :type function))
  (:documentation "High-level NSQ client with a WORKER thread"))

;;

(defmethod initialize-instance :after ((obj nsq) &key)
  (with-slots (host port connect-timeout parameters) obj
    (setf (slot-value obj 'connection)
	  (connect :host host :port port :timeout connect-timeout))
    (setf (slot-value obj 'lock)
	  (make-lock))
    (setf (slot-value obj 'response-queue)
	  (make-instance 'bounded-channel :size 1))
    ;; worker should be running before any command will be issue
    (setf (slot-value obj 'worker)
	  (make-thread #'(lambda () (pump obj))
		       :name "nsq-worker"))
    ;;
    (identify obj parameters)))

;;

(defmethod identify ((obj nsq) &optional parameters)
  (with-slots (connection lock response-queue) obj
    (with-lock-held (lock)
      (write-frame :identify (json:encode (or parameters :empty) :octets t)
		   (socket-stream connection)))
    (recv response-queue)))

(defmethod nop ((obj nsq))
  (with-slots (connection lock) obj
    (with-lock-held (lock)
      (write-frame :nop nil (socket-stream connection)))))

(defmethod close ((obj nsq) &key abort)
  (declare (ignore abort))
  (with-lock-held ((slot-value obj 'lock))
    (with-slots (connection mode worker response-queue) obj
      (when (eq mode :subscriber)
	(write-frame :cls nil (socket-stream connection))
	(recv response-queue))
      (socket-close connection)
      (and (thread-alive-p worker)
	   (join-thread worker)))))

;;

(defmethod publish ((obj nsq) topic payload)
  (with-slots (connection lock response-queue) obj
    (with-lock-held (lock)
      (let ((mode (slot-value obj 'mode)))
	(cond ((eq mode :publisher))
	      ((not mode) (setf (slot-value obj 'mode) :publisher))
	      (t (error (concatenate
			 'string
			 (format nil "already working in ~a mode, " mode)
			 "consider creating a separate NSQ client instance")))))
      (write-frame :pub payload (socket-stream connection)
		   :arguments (list topic)))
    (recv response-queue)))

(defmethod subscribe ((obj nsq) topic channel handler)
  (with-slots (connection lock response-queue) obj
    (with-lock-held (lock)
      (let ((mode (slot-value obj 'mode)))
	(when mode
	  (error (concatenate
		  'string
		  (if (eq mode :subscriber)
		      "multiple subscriptions are prohibited by the protocol, "
		      (format nil "already working in ~a mode, " mode))
		  "consider creating a separate NSQ client instance"))))
      (setf (slot-value obj 'mode) :subscriber)
      (setf (slot-value obj 'handler) handler)
      (write-frame :sub nil (socket-stream connection)
		   :arguments (list topic channel)))
    (recv response-queue)))

;;

(defmethod ready ((obj nsq) (amount integer))
  (with-slots (lock connection) obj
    (with-lock-held (lock)
      (write-frame :rdy nil (socket-stream connection)
		   :arguments (list (write-to-string amount))))))

(defmethod finish ((obj nsq) (m message))
  (with-slots (lock connection) obj
    (with-lock-held (lock)
      (write-frame :fin nil (socket-stream connection)
		   :arguments (list (int->hex (slot-value m 'id)))))))

(defmethod requeue ((obj nsq) (m message) timeout)
  (with-slots (lock connection) obj
    (with-lock-held (lock)
      (write-frame :req nil (socket-stream connection)
		   :arguments (list (int->hex (slot-value m 'id)) timeout)))))

(defmethod touch ((obj nsq) (m message))
  (with-slots (lock connection) obj
    (with-lock-held (lock)
      (write-frame :touch nil (socket-stream connection)
		   :arguments (list (int->hex (slot-value m 'id)))))))

;;

(defmethod pump ((obj nsq))
  (with-slots (connection parameters response-queue) obj
    (let* ((next #'(lambda (stream)
		     (dispatch-frame (read-frame stream))))
	   (stream (socket-stream connection))
	   (current (funcall next stream)))
      (do () ((not current) nil)
	(cond ((eq 'heartbeat current)
	       (nop obj))
	      ((message-p current)
	       (let ((handler (or (slot-value-safe obj 'handler)
				  (error "got a message to handle, but no handler defined"))))
		 (funcall handler current)
		 (when (not (eq :eof (peek-byte (message-stream current) nil nil :eof)))
		   (error (format nil "~a stream is not drain" (type-of current))))))
	      (t (send response-queue current)))
	(setq current (funcall next stream))))))

;;

(defun make-nsq (host port &key (parameters nil))
  "Construct NSQ instance with specified HOST address and PORT number"
  (make-instance 'nsq :host host :port port :parameters parameters))


;;;;

;; (defvar nsq-client-sub)
;; (defvar sub-n)
;; (setq sub-n 0)
;; (setf nsq-client-sub (make-nsq "127.0.0.1" 4150 :parameters '(:|heartbeat_interval| 10000)))

;; (subscribe nsq-client-sub "test" "ephemeral-channel-1"
;; 	   #'(lambda (message)
;; 	       (read-stream-content-into-string (message-stream message))
;; 	       (setq sub-n (+ 1 sub-n))
;; 	       (finish nsq-client-sub message)))
;; (ready nsq-client-sub 1024)
;; ;;(close nsq-client-sub)

;; ;;

;; (defvar nsq-client-pub)
;; (setf nsq-client-pub (make-nsq "127.0.0.1" 4150 :parameters '(:|heartbeat_interval| 10000)))

;; (time (loop :repeat 1000000
;; 	    :do (publish nsq-client-pub "test" "hi")))
;; ;;(close nsq-client-pub)
