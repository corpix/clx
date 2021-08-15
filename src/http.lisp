(in-package #:cl-user)
(defpackage clx/http
  (:use #:clx/std #:clx/uri)
  (:export #:request
	   #:request-method
	   #:request-uri
	   #:request-content
	   #:request-headers
	   #:request-p
	   #:make-request

	   #:do-request))
(in-package #:clx/http)

(reexport-from :dexador :exclude
	       '(:get :post :head :put :patch :delete :request
		 :request-uri :request-method))

;;

(defstruct request
  "Represents a prepared HTTP request"
  (method :get :type (or keyword symbol))
  (uri nil :type (or uri string))
  (content nil)
  (headers nil))

(defun do-request (req)
  (let ((u (request-uri req)))
    (dex:request (if (uri-p u) u (uri u))
		 :method (request-method req)
		 :headers (request-headers req)
		 :content (request-content req))))
