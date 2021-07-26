(in-package :cl-user)
(defpackage clx/apps/main
  (:use :cl)
  (:export :main :*version*))
(in-package :clx/apps/main)

(defparameter *version* "dev")

(defun main ()
  (format t "hello world ~a" *version*)
  (fresh-line))
