(in-package :cl-user)
(defpackage clx/tests/clickhouse
  (:use :clx/std :rove)
  (:local-nicknames (:ch :clx/clickhouse)))
(in-package :clx/tests/clickhouse)

(deftest create
    (testing "create/database"
      (ok (equalp (ch:create/database "foo")
		  #s(ch::ast/create/database
		     :name "foo"
		     :idempotent? nil
		     :cluster nil
		     :engine nil)))))
