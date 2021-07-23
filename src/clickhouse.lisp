(defpackage clx/clickhouse
  (:use :cl))
(in-package :clx/clickhouse)

(defstruct ast/create-table)
(defstruct ast/insert)
(defstruct ast/select)

;;

(defmacro create-table)
(defmacro insert)
(defmacro select)
