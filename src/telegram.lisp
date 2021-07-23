(in-package :cl-user)
(defpackage clx/telegram
  (:nicknames :clx/tg)
  (:use :cl)
  (:import-from :trivia :let-match*)
  (:export

   :*token*
   :*url*
   :*timeout*
   :*poll-timeout*
   :*log*
   :token-is-not-set

   :defbotapi
   :get-me
   :get-updates
   :send-message
   :send-photo))
(in-package :clx/telegram)

(defparameter *token* "")
(defparameter *url* "https://api.telegram.org")
(defparameter *timeout* 5)
(defparameter *poll-timeout* 120)
(defparameter *log* (lambda (&rest xs) (print xs)))

;;

(define-condition token-is-not-set (error)
  ()
  (:documentation "Telegram Bot API token should be set")
  (:report (lambda (condition stream)
	     (format stream "Telegram Bot API token should be defined via PARAMETER *token*, currently it's value is ~s"
		     *token*))))

;;

(defun uri (method)
  "Construct URI to call a Telegram Bot API method"
  (when (equalp "" *token*)
    (error 'token-is-not-set))
  (quri:uri (concatenate 'string *url* "/bot" *token* "/" method)))

;;

(defun encode-query (req parameters)
  (setf (clx/http:request-uri req)
	(apply #'clx/http:uri-with-query-params
	       (clx/http:request-uri req)
	       parameters))
  req)

;; FIXME: there is no 'encode-json' func
;; because content type automagically infered by dexador :(
;; maybe port my http client from scheme/improve dexador?
(defun encode-body (req parameters)
  (setf (clx/http:request-content req)
	parameters)
  req)

(defun decode-json (payload)
  (jojo:parse payload))

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
	 (let* ((req (clx/http:make-request
		      :uri (uri ,api-method)
		      :method ,request-method))
		(parameters (list ,@keyword-parameters))
		(encode ,encode)
		(decode ,decode)
		(result (clx/http:do-request
			    (if encode
				(funcall encode req parameters)
				req))))
	   (if decode (funcall decode result) result))))))

;;

(defbotapi get-me ()
  (('GET "getMe")
   (nil #'decode-json)))

(defbotapi get-updates ((limit 100) offset (timeout *poll-timeout*))
  (('GET "getUpdates")
   (#'encode-query #'decode-json)))

(defbotapi send-message (chat_id text)
  (('POST "sendMessage")
   (#'encode-body #'decode-json)))

(defbotapi send-photo (chat_id photo (caption ""))
  (('POST "sendPhoto")
   (#'encode-body #'decode-json)))

;;

;; (get-me)
;; (get-updates)
;; (send-message :chat_id 82935604 :text "hello")
;; (send-photo :chat_id 82935604 :caption "hello" :photo #p"./src/sempai.jpg")
