;;; -*- Lisp -*-
#-asdf3 (error "fare-quasiquote requires ASDF 3")

(defsystem "fare-quasiquote-readtable"
  :description "Using fare-quasiquote with named-readtable"
  :version (:read-file-line "version.text")
  :license "MIT"
  :author "Francois-Rene Rideau"
  :depends-on ("named-readtables" "fare-quasiquote")
  :components ((:file "quasiquote-readtable")))
