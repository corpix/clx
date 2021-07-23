(in-package :cl-user)
(defpackage clx/tests/main
  (:use :cl :rove)
  (:export :main))
(in-package :clx/tests/main)

(deftest sample-test
    (testing "sample test"
	     (ok (= 1 1))))
