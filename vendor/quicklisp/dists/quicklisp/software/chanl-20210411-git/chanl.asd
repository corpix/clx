;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan, Adlai Chandrasekhar
;;;;
;;;; Another System Definition
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsystem "chanl"
  :name "chanl"
  :author "Kat Marchan"
  :description "Communicating Sequential Process support for Common Lisp"
  :depends-on ("bordeaux-threads")
  :components
  ((:module "src"
            :serial t
            :components ((:file "trivial-cas")
                         (:file "package")
                         (:file "utils")
                         (:file "threads")
                         (:file "queues")
                         (:file "channels")
                         (:file "select"))))
  :in-order-to ((test-op (test-op "chanl/tests"))))

(defsystem "chanl/examples"
  :name "chanl examples"
  :maintainer "Adlai Chandrasekhar"
  :author "Kat Marchan"
  :description "Examples of how to use chanl"
  :depends-on ("chanl")
  :serial t
  :components
  ((:module "examples"
            :serial t
            :components ((:file "package")
                         (:file "utils")
;(cerror "READ THE CODE")(:file "actors")
; (cerror "FIXME")       (:file "actors-tests")
                         (:file "conditions")
                         (:file "sieve")
                         (:file "futures")))))

(defsystem "chanl/tests"
  :name "chanl tests"
  :maintainer "Adlai Chandrasekhar"
  :description "Unit Tests for the ChanL library and its examples"
  :depends-on ("chanl" "fiveam")
  :serial t
  :components
  ((:module "tests"
            :serial t
            :components ((:file "setup-tests")
                         (:file "queues")
                         (:file "channels")
                         (:file "select"))))
  :perform
  (test-op (o c)
    (format t "~2&*******************~@
                  ** Starting test **~@
                  *******************~%")
    (handler-bind ((style-warning #'muffle-warning))
      (symbol-call :chanl :run-all-tests))
    (format t "~2&*****************************************~@
                  **            Tests finished           **~@
                  *****************************************~@
                  ** If there were any failures, please  **~@
                  **      file a bugreport on github:    **~@
                  **     github.com/zkat/chanl/issues    **~@
                  *****************************************~%")))
