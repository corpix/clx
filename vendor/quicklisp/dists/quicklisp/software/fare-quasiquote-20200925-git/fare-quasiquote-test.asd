;;; -*- Lisp -*-

(defsystem "fare-quasiquote-test"
  :description "Tests for fare-quasiquote"
  :version (:read-file-line "version.text")
  :license "MIT"
  :author "Francois-Rene Rideau"
  :depends-on ("fare-quasiquote-extras" "hu.dwim.stefil")
  :components ((:file "quasiquote-test"))
  :perform (test-op (o c)
             (format! t "~&Testing fare-quasiquote")
             (symbol-call :fare-quasiquote/test :fare-quasiquote-test)))
