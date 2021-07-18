;; -*- mode: lisp -*-
(defsystem :clx
  :version "0.1.0"
  :author "Dmitry Moskowski"
  :license "public domain"
  :depends-on ("alexandria")
  :components ((:file "clickhouse"))
  :in-order-to ((test-op (test-op "corpix/clx/tests"))))
