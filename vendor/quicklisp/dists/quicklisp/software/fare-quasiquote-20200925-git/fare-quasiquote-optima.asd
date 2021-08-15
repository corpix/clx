;;; -*- Lisp -*-
#-asdf3 (error "fare-quasiquote requires ASDF 3")

(defsystem "fare-quasiquote-optima"
  :description "fare-quasiquote extension for optima"
  :version (:read-file-line "version.text")
  :license "MIT"
  :author "Francois-Rene Rideau"
  :depends-on ("trivia.quasiquote"))
