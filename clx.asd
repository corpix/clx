;; -*- mode: lisp -*-
(defsystem :clx
  :version "0.1.0"
  :author "Dmitry Moskowski"
  :license "public domain"
  :depends-on (:alexandria
	       :lisp-binary
	       :babel
	       :iterate
	       :ironclad
	       :trivia
	       :usocket
	       :cl-async
	       :dexador
	       :quri
	       :jonathan
	       :cffi
	       :cffi-grovel)
  :components ((:module "src"
		:components (;;(:file "clickhouse")
			     (:file "http")
			     (:file "telegram")))))

(defsystem :clx/tests
  :version "0.1.0"
  :author "Dmitry Moskowski"
  :license "public domain"
  :depends-on (:rove
	       :clx)
  :components ((:module "src/tests"
		:components ((:file "main")))))

(defsystem :clx/apps/main
  :version "0.1.0"
  :author "Dmitry Moskowski"
  :license "public domain"
  :depends-on (:clx)
  :components ((:module "src/apps"
		:components ((:file "main")))))
