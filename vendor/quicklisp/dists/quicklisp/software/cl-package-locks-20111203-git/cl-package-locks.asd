;;;; cl-package-locks.asd

(asdf:defsystem #:cl-package-locks
  :serial t
  :version "0.0.2"
  :author "Elliott Johnson <elliott@elliottjohnson.net>"
  :license "Copyright 2011 Elliott Johnson"
  :description "A library to provide a unified way to work with package locks across supported common lisp implementations."
  :components ((:file "package")
               (:file "cl-package-locks")))

