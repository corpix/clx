(in-package :cl-user)
(defpackage clx/json
  (:use :clx/std)
  (:export
   :encode
   :decode))
(in-package :clx/json)

(defmacro encode (value &rest rest)
  `(jojo:to-json ,value ,@rest))

(defmacro decode (sequence &rest rest)
  `(jojo:parse ,sequence ,@rest))
