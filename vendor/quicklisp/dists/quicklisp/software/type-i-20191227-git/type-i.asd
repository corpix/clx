#|
  This file is a part of type-i project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  Author: Masataro Asai (guicho2.71828@gmail.com)
|#

(defsystem type-i
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:introspect-environment
               :alexandria
               :trivia.trivial
               :lisp-namespace)
  :pathname "src/"
  :components ((:file "package")
               (:file "infer-typep")
               (:file "infer-unary")
               (:file "infer-derived")
               (:file "infer-constants")
               (:file "infer-compound")
               (:file "infer-numbers"))
  :serial t
  :description "Type Inference Utility on Fundamentally 1-arg Predicates"
  :in-order-to ((test-op (load-op type-i.test))))
