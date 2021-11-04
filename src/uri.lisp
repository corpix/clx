(in-package :cl-user)
(defpackage clx/uri
  (:use :clx/std)
  (:export :uri-with-query-params))
(in-package :clx/uri)

(reexport-from :quri)

;;

;; FIXME: copy uri
(defun uri-with-query-params (subject &rest query)
  "Set QUERY parameters into URI and return"
  (setf (quri:uri-query-params subject) query)
  subject)
