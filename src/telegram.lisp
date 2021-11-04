(in-package :cl-user)
(defpackage clx/telegram
  (:nicknames :clx/tg)
  (:use
   :clx/std
   :clx/http
   :clx/uri)
  (:local-nicknames (:json :clx/json))
  (:import-from
   :trivia
   :let-match*)
  (:export
   :*token*
   :*api*
   :*poll-timeout*
   :token-required

   :defbotapi
   :get-me
   :get-updates
   :send-message
   :send-photo
   :send-document))
(in-package #:clx/telegram)

(defparameter *token* "")
(defparameter *api* "https://api.telegram.org")
(defparameter *poll-timeout* 120)

;;

(define-condition token-required (error)
  ()
  (:documentation "Telegram Bot API token should be defined")
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (format stream "Telegram Bot API token should be defined via PARAMETER *token*, currently it's value is ~s"
		     *token*))))

;;

(defun method-uri (method)
  "Construct URI to call a Telegram Bot API method"
  (when (equalp "" *token*)
    (error 'token-required))
  (uri (concatenate 'string *api* "/bot" *token* "/" method)))

;;

(defun encode-query (req parameters)
  (setf (request-uri req)
	(apply #'uri-with-query-params
	       (request-uri req)
	       parameters))
  req)

(defun encode-body (req parameters)
  (setf (request-content req) parameters)
  req)

(defun decode-body (payload)
  (json:decode payload))

;;

(defmacro defbotapi (name parameters meta)
  "Define Telegram Bot API method with input PARAMETERS as a procedure NAME."
  (let ((keyword-parameters
	  (loop :for value :in parameters
		:collect (let ((sym (cond ((listp value) (car value))
					  (t value))))
			   (list 'cons (string-downcase (symbol-name sym))
				 sym)))))
    (let-match* (((list descriptor transformers) meta)
		 ((list request-method api-method) descriptor)
		 ((list encode decode) transformers))
      `(defun ,name ,(and parameters `(&key ,@parameters))
	 (let* ((req (make-request
		      :uri (method-uri ,api-method)
		      :method ,request-method))
		(parameters (list ,@keyword-parameters))
		(encode ,encode)
		(decode ,decode)
		(result (do-request
			    (if encode
				(funcall encode req parameters)
				req))))
	   (if decode (funcall decode result) result))))))

;;

(defbotapi get-me ()
  (('GET "getMe")
   (nil #'decode-body)))

(defbotapi get-updates ((limit 100) offset (timeout *poll-timeout*))
  (('GET "getUpdates")
   (#'encode-query #'decode-body)))

(defbotapi send-message (chat_id text)
  (('POST "sendMessage")
   (#'encode-body #'decode-body)))

(defbotapi send-photo (chat_id photo (caption ""))
  (('POST "sendPhoto")
   (#'encode-body #'decode-body)))

(defbotapi send-document (chat_id document (caption ""))
  (('POST "sendDocument")
   (#'encode-body #'decode-body)))

;;

;; (get-me)
;; (get-updates)
;; (send-message :chat_id 82935604 :text "hello")
;; (send-photo :chat_id 82935604 :caption "hello" :photo #p"./src/sempai.jpg")
