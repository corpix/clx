;;; -*- Lisp -*-
#-asdf3 (error "fare-quasiquote requires ASDF 3")

(defsystem "fare-quasiquote-extras"
  :description "fare-quasiquote plus extras"
  :version (:read-file-line "version.text")
  :license "MIT"
  :author "Francois-Rene Rideau"
  ;; NB: not including deprecated fare-matcher anymore
  :depends-on ("fare-quasiquote-optima" "fare-quasiquote-readtable"))
