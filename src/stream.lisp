(in-package :cl-user)
(defpackage clx/stream
  (:use :clx/std)
  (:export :*null-stream*))
(in-package :clx/stream)

(reexport-from :flexi-streams :exclude '(:octet))

;;

(defparameter *null-stream* (make-broadcast-stream)
  "Stream which serves a purpose of /dev/null")
