(in-package :cl-user)
(defpackage clx/apps/main
  (:use :cl)
  (:export :main))
(in-package :clx/apps/main)

(defun main ()
  (print "hello world")
  (fresh-line))
