(in-package :cl-user)
(defpackage clx/sql
  (:use :clx/std)
  (:export))
(in-package :clx/sql)

;;

(defmacro defsql (sym args &body body)
  (flet ((make-fields (args)
	   (remove-if (lambda (arg) (eq arg '&key)) args))
	 (make-arguments (args)
	   (multiple-value-bind (positional keyword)
	       (let ((pos (position '&key args)))
		 (if pos
		     (halves args pos)
		     (values args nil)))
	     `(,@(mapcar #'car positional)
	       ,@(mapcar #'(lambda (arg)
			     (if (sequencep arg)
				 (take 2 arg)
				 arg))
			 keyword)))))
    (let* ((fields (make-fields args))
	   (arguments (make-arguments args)))
      (flet ((make-struct-ctor (sym fields)
	       ;; `(,(intern "MAKE-...") ,:key1 ,value1 ,:key2 ,value2 ...)
	       `(`(,(intern ,(string-upcase (concatenate 'string "make-" (symbol-name sym))))
		   ,,@(reduce #'(lambda (acc field)
				  (append acc (list (make-keyword (car field))
						    (car field))))
			      fields
			      :initial-value nil)))))
	`(progn
	   (defstruct ,sym
	     ,@fields)
	   (defmacro ,sym ,arguments
	     ,@(or body (make-struct-ctor sym fields))))))))

;;

(defsql sql-expression ((value nil :type (or (cons sql-expression)
					     number string boolean
					     symbol keyword null)))
  `(labels ((recur (value)
	      (make-sql-expression
	       :value (econd
			((or (numberp value) (stringp value)
			     (keywordp value) (symbolp value)
			     (eq value nil) (eq value t))
			 value)
			((listp value)
			 (mapcar #'recur value))))))
     (recur ',value)))

(defmethod display ((sql sql-expression))
  (let ((value (sql-expression-value sql)))
    (econd
      ((or (symbolp value)
	   (keywordp value))
       (string value))
      ((stringp value)
       (concatenate 'string "'" value "'"))
      ((numberp value)
       (write-to-string value))
      ((eq value t) "1")
      ((eq value nil) "0")
      ((listp value)
       (concatenate 'string
		    (display (car value))
		    "(" (string-join (mapcar #'display (cdr value)) ", ") ")")))))

(sql-expression (+ 1 1))
;; => #S(SQL-EXPRESSION
;;      :VALUE (#S(SQL-EXPRESSION :VALUE +) #S(SQL-EXPRESSION :VALUE 1)
;;              #S(SQL-EXPRESSION :VALUE 1)))
(display (sql-expression (plus 1 1)))
;; => "PLUS(1, 1)"

;;

(defsql sql-columns ((list nil :type (or (cons (or string symbol keyword)) null))))

(defmethod display ((sql sql-columns))
  (string-join (mapcar #'string (sql-columns-list sql)) ", "))

(sql-columns '(foo bar))
;; => #S(SQL-COLUMNS :LIST (FOO BAR))
(display (sql-columns '(foo bar)))
;; => "FOO, BAR"

;;

(defsql sql-select ((columns nil :type (or sql-columns null))
		    &key
		    (from nil :type (or string symbol keyword null)))
  `(make-sql-select
    :columns (sql-columns ,columns)
    :from ,from))

(defmethod display ((sql sql-select))
  (with-slots (columns from) sql
    (print from)
    (concatenate 'string
		 "select " (display columns)
		 (and from " from ")
		 (and from (string from)))))

;;

(sql-select '(foo bar))
;; => #S(SQL-SELECT :COLUMNS #S(SQL-COLUMNS :LIST (FOO BAR)))
(display (sql-select '(foo bar)))
;; => "select FOO, BAR"

(sql-select '(foo bar) :from 'baz)
;; => #S(SQL-SELECT :COLUMNS #S(SQL-COLUMNS :LIST (FOO BAR)) :FROM BAZ)
(display (sql-select '(foo bar) :from 'baz))
;; => "select FOO, BAR from BAZ"
