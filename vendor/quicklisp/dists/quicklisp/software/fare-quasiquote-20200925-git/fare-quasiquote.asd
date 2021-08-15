;;; -*- Lisp -*-
#-asdf3 (error "fare-quasiquote requires ASDF 3")

(defsystem "fare-quasiquote"
  :description "Portable, matchable implementation of quasiquote"
  :long-description "fare-quasiquote implements
  a portable quasiquote that you can control."
  :license "MIT"
  :author "Francois-Rene Rideau"
  :version (:read-file-line "version.text")
  :depends-on ((:version "fare-utils" "1.0.0"))
  :components
  ((:file "packages")
   (:file "quasiquote" :depends-on ("packages"))
   (:file "pp-quasiquote" :depends-on ("quasiquote")))
  :in-order-to ((test-op (test-op "fare-quasiquote-test"))))
