(in-package :cl-user)
(defpackage clx/rdb
  (:use :clx/std)
  (:export))
(in-package :clx/rdb)

;;

(defvar *db*)
(setf *db* (mito:connect-toplevel :sqlite3 :database-name "state.db"))

;; (mito:deftable message ()
;;   ((from :col-type (:varchar 64))
;;    (text :col-type :text)))

(defclass message ()
  ((from :col-type (:varchar 64)
	 :accessor message-from)
   (text :col-type :text
	 :accessor message-text))
  (:metaclass mito:dao-table-class))

(sxql:yield (car (mito:table-definition 'message)))
(mito:execute-sql (car (mito:table-definition 'message)))

(defvar incoming (make-instance 'message :from "corpix" :text "hello you"))

(mito:insert-dao incoming)
(mito:object-id incoming)
(mito:object-created-at incoming)
(mito:object-updated-at incoming)
(mito:find-dao 'message :id 1)
(mito:save-dao incoming)
(mito:delete-dao incoming)
(mito:count-dao 'message)

(prog ()
   (go :baz)
 :foo
   (print "listen what i say yo")
   (go :end)
 :baz
   (print "hey")
   (go :bar)
 :bar
   (print "you")
   (go :foo)
 :end)
(force-output *standard-output*)

;; https://github.com/fukamachi/mito#relationship
