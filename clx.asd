;; -*- mode: lisp -*-
(defsystem :clx
  :version "0.1.0"
  :author "Dmitry Moskowski"
  :license "public domain"
  :depends-on (:cl-reexport
	       :cl-octet-streams
	       :cl-ppcre
	       :cl-intbytes
	       :cl-gserver
	       :bit-smasher
	       :arrows
	       :alexandria
	       :serapeum
	       :bordeaux-threads
	       :lparallel
	       :event-emitter
	       :chanl
	       :lisp-binary
	       :str
	       :trivia
	       :usocket
	       :uiop
	       :iterate
	       :babel
	       :local-time
	       :local-time-duration
	       :flexi-streams
	       :ironclad
	       :dexador
	       :websocket-driver
	       :quri
	       :jonathan
	       :mito
	       :cffi
	       :cffi-grovel)
  :serial t
  :components ((:module "src"
		:components ((:file "std")
			     (:file "stream")
			     (:file "binary")
			     (:file "channel")
			     (:file "process")
			     (:file "fs")
			     (:file "json")
			     (:file "time")
			     (:file "uri")
			     (:file "http")
			     (:file "telegram")
			     (:file "nsq")
			     ;; (:file "clickhouse")
			     ))))

(defsystem :clx/tests
  :version "0.1.0"
  :author "Dmitry Moskowski"
  :license "public domain"
  :depends-on (:rove
	       :clx)
  :serial t
  :components ((:module "src/tests"
		:components ((:file "main")
			     ;; (:file "clickhouse")
			     ))))

(defsystem :clx/apps/main
  :version "0.1.0"
  :author "Dmitry Moskowski"
  :license "public domain"
  :depends-on (:clx)
  :components ((:module "src/apps"
		:components ((:file "main")))))
