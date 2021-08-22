(in-package #:cl-user)
(defpackage clx/clickhouse
  (:use #:clx/std)
  (:import-from #:trivia
		#:match
		#:ematch
		#:guard)
  (:export #:create
	   #:create/database))
(in-package #:clx/clickhouse)

;; https://clickhouse.tech/docs/en/sql-reference/
;; https://clickhouse.tech/docs/en/sql-reference/statements/create/
;; https://clickhouse.tech/docs/en/engines/database-engines/

(defstruct ast/expression
  (value nil :type (or (cons ast/expression)
		       number
		       string
		       boolean
		       symbol
		       keyword
		       null)))

(defmacro expression (expr)
  (labels ((recur (expr)
	     `(make-ast/expression
	       :value ,(ematch expr
			 ((guard v (or (numberp v)
				       (stringp v)
				       (keywordp v))) v)
			 ((guard v (symbolp v)) `(quote ,v))
			 ((guard v (or (eq v nil)
				       (eq v t))) v)
			 ((list* v)
			  (cons 'list (mapcar #'recur v)))))))
    (recur expr)))

(defun emit/expression (tree)
  (ematch tree
    ((ast/expression (value (guard v (stringp v)))) v)
    ((ast/expression (value (guard v (numberp v))))
     (write-to-string v))
    ((ast/expression (value (guard v (eq v nil)))) "0")
    ((ast/expression (value (guard v (eq v t)))) "1")
    ((ast/expression (value (guard v (or (keywordp v)
					 (symbolp v)))))
     (string v))
    ((ast/expression (value (list* fn v)))
     (concatenate 'string
		  (emit/expression fn)
		  "("
		  (if (> (length v) 0)
		      (string-join (mapcar #'emit/expression v) ", ")
		      "")
		  ")"))))

(emit/expression (expression 1)) ;; => "1"
(emit/expression (expression (1 2 3))) ;; => "1(2, 3)" ;; not valid, but not our duty to check
(emit/expression (expression t))   ;; => "1"
(emit/expression (expression nil)) ;; => "0"
(emit/expression (expression (plus 1 (multiply 2 3 4 5 6)))) ;; => "PLUS(1, MULTIPLY(2, 3, 4, 5, 6))"

(defstruct ast/engine/atomic)
(defstruct ast/engine/mysql)
(defstruct ast/engine/materialized-mysql)
(defstruct ast/engine/lazy)
(defstruct ast/engine/postgresql)
(defstruct ast/engine/replicated)

(defstruct ast/engine
  (statement (make-ast/engine/atomic)
   :type (or ast/engine/atomic
	     ast/engine/mysql
	     ast/engine/materialized-mysql
	     ast/engine/lazy
	     ast/engine/postgresql
	     ast/engine/replicated)))

(defmacro engine (type &rest args)
  `(make-ast/engine
    :statement ,(ecase type
		  (:atomic `(make-ast/engine/atomic ,@args))
		  (:mysql)
		  (:materialized-mysql)
		  (:postgresql)
		  (:replicated))))

(defun emit/engine (tree)
  (ematch tree
    ((ast/engine/atomic) "atomic")
    ((ast/engine/mysql))
    ((ast/engine/materialized-mysql))
    ((ast/engine/lazy))
    ((ast/engine/postgresql))
    ((ast/engine/replicated))))

;;

(defstruct ast/create/database
  (name nil :type (or string null))
  (idempotent? nil :type boolean)
  (cluster nil :type (or string null))
  (engine nil :type (or ast/engine null)))

(defmacro create/database (name &key (idempotent? nil) (cluster nil) (engine nil))
  `(make-ast/create/database :name ,name
			     :idempotent? ,idempotent?
			     :cluster ,cluster
			     :engine ,(and engine `(engine ,@engine))))

;;

(defstruct ast/column
  (name nil :type (or string null))
  (type nil :type (or ast/expression null))
  (null? t :type boolean)
  (default nil :type (or ast/expression null))
  (materialized nil :type (or ast/expression null))
  (alias nil :type (or ast/expression null))
  (compression nil :type (or ast/expression null))
  (ttl nil :type (or ast/expression null)))

(defmacro column (name &key (type nil) (null? t) (default nil) (materialized nil) (alias nil) (compression nil) (ttl nil))
  `(make-ast/column :name ,name
		    :type ,(and type `(expression ,type))
		    :null? ,null?
		    :default ,(and default `(expression ,default))
		    :materialized ,(and materialized `(expression ,materialized))
		    :alias ,(and alias `(expression ,alias))
		    :compression ,(and compression `(expression ,compression))
		    :ttl ,(and ttl `(expression ,ttl))))

(defmacro columns (&rest columns)
  `(list ,@(mapcar #'(lambda (column) `(column ,@column)) columns)))

(defun emit/column (tree)
  (ematch tree
    ((ast/column name type null? default materialized alias compression ttl)
     (concatenate 'string
		  (string name)
		  (if type (concatenate 'string " " (emit/expression type)) "")
		  (if null? " NULL" " NOT NULL")
		  (if default (concatenate 'string " DEFAULT " (emit/expression default)) "")
		  (if materialized (concatenate 'string " MATERIALIZED " (emit/expression materialized)) "")
		  (if alias (concatenate 'string " ALIAS " (emit/expression alias)) "")
		  (if compression (concatenate 'string " CODEC(" (emit/expression compression) ")") "")
		  (if ttl (concatenate 'string " TTL " (emit/expression ttl)) "")))))

(defun emit/columns (tree)
  (concatenate 'string
	       "(" (string-join (mapcar #'emit/column tree) ", ") ")"))

;;

(defstruct ast/create/table
  (name nil :type (or string null))
  (database nil :type (or string null))
  (idempotent? nil :type boolean)
  (cluster nil :type (or string null))
  (columns nil :type (or (cons ast/column) null))
  (engine nil :type (or ast/engine null)))

(defmacro create/table (name &key (database nil) (idempotent? nil) (cluster nil) (columns nil) (engine nil))
  `(make-ast/create/table :name ,name
			  :database ,database
			  :idempotent? ,idempotent?
			  :cluster ,cluster
			  :columns ,(and columns `(columns ,@columns))
			  :engine ,(and engine `(engine ,@engine))))

;;

(defstruct ast/create/view)
(defstruct ast/create/dictionary)
(defstruct ast/create/user)
(defstruct ast/create/role)
(defstruct ast/create/row-policy)
(defstruct ast/create/quota)
(defstruct ast/create/settings-profile)

(defstruct ast/create
  (statement nil :type (or null
			   ast/create/database
			   ast/create/table
			   ast/create/view
			   ast/create/dictionary
			   ast/create/user
			   ast/create/role
			   ast/create/row-policy
			   ast/create/quota
			   ast/create/settings-profile)))

(defmacro create (type &rest args)
  `(make-ast/create
    :statement ,(ecase type
		  (:database `(create/database ,@args))
		  (:table `(create/table ,@args))
		  (:view)
		  (:dictionary)
		  (:user)
		  (:role)
		  (:row-policy)
		  (:quota)
		  (:settings-profile))))

(defun emit/create (tree)
  (concatenate 'string
	       (ematch tree
		 ((ast/create/database name idempotent? cluster engine)
		  (concatenate 'string
			       "DATABASE "
			       (if idempotent? "IF NOT EXISTS " "")
			       (string name)
			       (if cluster (concatenate 'string " ON CLUSTER " cluster) "")
			       (if engine (concatenate 'string " ENGINE = " (emit/engine engine)) "")))
		 ((ast/create/table database name idempotent? cluster columns engine)
		  (concatenate 'string
			       "TABLE "
			       (if database (concatenate 'string database ". ") "")
			       (if idempotent? "IF NOT EXISTS " "")
			       (string name)
			       (if cluster (concatenate 'string " ON CLUSTER " cluster) "")
			       (if columns (concatenate 'string " " (emit/columns columns)) "")
			       (if engine (concatenate 'string " ENGINE = " (emit/engine engine)) ""))))))

;;

(defstruct ast/insert
  (database nil :type (or null string))
  (table nil :type (or string null))
  (columns nil :type (or null (cons keyword)))
  (values nil :type (or null (cons (cons (or string number)))))
  (format nil :type (or null keyword)))
(defstruct ast/select)

;;

(defun emit (tree)
  (ematch tree
    ((ast/create statement)
     (concatenate 'string "CREATE " (emit/create statement)))))

;;


(emit (create :database "hello" :idempotent? t))
;; => "CREATE DATABASE IF NOT EXISTS hello"
(emit (create :table "foo" :columns (("hello" :type :|Uint16|))
	      :idempotent? t))
;; => "CREATE TABLE IF NOT EXISTS foo (hello Uint16 NULL)"
(emit (create :table "foo" :columns (("hello"  :type :|Uint16|)
				     ("hello2" :type (:|FixedString| 16))
				     ("hello3" :type (array string))
				     ("time"   :type :|DateTime| :null? nil :default (now) :compression (lz4hc 9)))))
;; => "CREATE TABLE foo (hello Uint16 NULL,
;;                       hello2 FixedString(16) NULL,
;;                       hello3 ARRAY(STRING) NULL,
;;                       time DateTime NOT NULL DEFAULT NOW() CODEC(LZ4HC(9)))"
