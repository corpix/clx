(in-package :cl-user)
(defpackage clx/http
  (:nicknames :clx/http)
  (:use :cl)
  (:export

   :*verbose*

   :request
   :request-method
   :request-uri
   :request-content
   :request-headers
   :request-p
   :make-request

   :uri
   :uri-with-query-params
   :do-request))
(in-package :clx/http)

(defparameter *verbose* nil)

(defstruct request
  "Represents a prepared HTTP request"
  method
  uri
  content
  headers)

(defun uri (u)
  "Parse URI with QURI or return unchanged if it is already parsed"
  (if (quri:uri-p u) u (quri:uri u)))

(defun uri-with-query-params (uri &rest query)
  "Set QUERY parameters into URI and return"
  (setf (quri:uri-query-params uri) query)
  uri)

(defun do-request (req)
  (dex:request (uri (request-uri req))
	       :verbose *verbose*
	       :method (request-method req)
	       :headers (request-headers req)
	       :content (request-content req)))
