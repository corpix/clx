;;;; pretty-printing of backquote expansions

#+xcvb (module (:depends-on ("quasiquote")))

;;;; This software is originally derived from the CMU CL system via SBCL.
;;;; CMU CL was written at Carnegie Mellon University and released into
;;;; the public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty.

(in-package :fare-quasiquote)

(defun pprint-starts-with-dot-or-at-p (form)
  (and
   (symbolp form)
   (let ((output (with-output-to-string (s)
                   (write form :stream s
                               :level (min 1 (or *print-level* 1))
                               :length (min 1 (or *print-length* 1))))))
     (and (plusp (length output))
          (or (char= (char output 0) #\.)
              (char= (char output 0) #\@))))))

(defstruct (x-unquote (:constructor make-x-unquote)) form) ;; represent the escape
(defmethod print-object ((x x-unquote) stream)
  (princ ". " stream) (write (make-unquote (x-unquote-form x)) :stream stream))

(defstruct (x-n-vector (:constructor make-x-n-vector)) n contents) ;; represent the escape
(defmethod print-object ((x x-n-vector) stream)
  (write-char #\# stream)
  (when (x-n-vector-n x) (write (x-n-vector-n x) :stream stream))
  (let* ((c (x-n-vector-contents x))
         (u (if (quasiquote-form-p c)
                (quasiquote-unexpand c)
                (list (make-unquote-splicing c)))))
    (write u :stream stream)))

(defun quasiquote-unexpand (x)
  (assert (quasiquote-form-p x))
  (quasiquote-unexpand-0 (car x) (cdr x)))

(defun quasiquote-unexpand-last (x)
  (quasiquote-unexpand-1
   #-quasiquote-strict-append 'unquote-splicing #+quasiquote-strict-append 'x-unquote
   (car (last x))))

(defun quasiquote-unexpand-0 (top x)
  (ecase top
    ((quote)
     (assert (length=n-p x 1))
     (car x))
    ((make-vector n-vector)
     (let ((form (cons top x)))
       (assert (valid-k-n-vector-p form))
       (make-x-n-vector :n (k-n-vector-n form) :contents (k-n-vector-contents form))))
    ((list)
     (mapcar #'(lambda (el) (quasiquote-unexpand-1 'unquote el)) x))
    ((list* cons)
     ;;(apply 'list* (mapcar #'(lambda (el) (quasiquote-unexpand-1 'unquote el)) x))
     (nconc (mapcar #'(lambda (el) (quasiquote-unexpand-1 'unquote el)) (butlast x))
            (quasiquote-unexpand-last x)))
    ((append)
     (append (apply 'append
                    (mapcar (lambda (el) (quasiquote-unexpand-1 'unquote-splicing el)) (butlast x)))
             (quasiquote-unexpand-last x)))
    ((nconc)
     (append (apply 'append
                    (mapcar (lambda (el) (quasiquote-unexpand-1 'unquote-nsplicing el)) (butlast x)))
             (quasiquote-unexpand-last x)))))

(defun quasiquote-unexpand-2 (top form)
  (ecase top
    ((unquote)
     (make-unquote form))
    ((x-unquote)
     (list (make-x-unquote :form form)))
    ((unquote-splicing)
     (list (make-unquote-splicing form)))
    ((unquote-nsplicing)
     (list (make-unquote-nsplicing form)))))

(defun quasiquote-unexpand-1 (top x)
  (cond
    ((literalp x)
     (ecase top
       ((nil) (kwote x))
       ((unquote x-unquote) x)
       ((unquote-splicing unquote-nsplicing) (list x))))
    ((atom x)
     (quasiquote-unexpand-2 top x))
    ((not (null (cdr (last x))))
     (error "found illegal dotted quasiquote form: ~S" x))
    ((and (member top '(unquote x-unquote))
          (member (car x) '(list list* cons append nconc quote make-vector n-vector)))
     (quasiquote-unexpand x))
    ((and (member top '(unquote-splicing unquote-nsplicing))
          (or (null x) (member (car x) '(list list* cons append nconc quote))))
     (quasiquote-unexpand x))
    (t
     (quasiquote-unexpand-2 top x))))

(defun pprint-quasiquote (stream form &rest noise)
  (declare (ignore noise))
  (princ #\` stream)
  (write (quasiquote-unexpand form) :stream stream))


(defun pprint-unquasiquote (stream form &rest noise)
  (declare (ignore noise))
  (block nil
    (flet ((punt ()
             ;; Given an invalid form. Instead of erroring out, revert to standard *p-p-d* (ugly).
             ;; Unhappily, there is no "call next method" here.
             (let ((*print-pprint-dispatch* (with-standard-io-syntax *print-pprint-dispatch*)))
               (write form :stream stream)
               (return))))
      (cond
        ((quasiquotep form)
         (write (macroexpand-1 form) :stream stream)
         (return))
        ((unquotep form)
         (let ((x (second form)))
           (when (or (quasiquote-form-p x) (literalp x))
             (write (quasiquote-unexpand x) :stream stream)
             (return)))
         (write-char #\, stream)
         (when (pprint-starts-with-dot-or-at-p (cadr form)) (write-char #\space stream)))
        ((unquote-splicing-p form)
         (write-string ",@" stream))
        ((unquote-nsplicing-p form)
         (write-string ",." stream))
        (t (punt)))
      (write (cadr form) :stream stream)))
  nil)

(defun enable-qq-pp (&key (priority 0) (table *print-pprint-dispatch*))
  ;; Printing the read-time forms
  (set-pprint-dispatch '(cl:cons (eql list)) 'pprint-quasiquote priority table)
  (set-pprint-dispatch '(cl:cons (eql list*)) 'pprint-quasiquote priority table)
  (set-pprint-dispatch '(cl:cons (eql cons)) 'pprint-quasiquote priority table)
  (set-pprint-dispatch '(cl:cons (eql append)) 'pprint-quasiquote priority table)
  (set-pprint-dispatch '(cl:cons (eql nconc)) 'pprint-quasiquote priority table)
  (set-pprint-dispatch '(cl:cons (eql make-vector)) 'pprint-quasiquote priority table)
  (set-pprint-dispatch '(cl:cons (eql n-vector)) 'pprint-quasiquote priority table)
  (set-pprint-dispatch '(cl:cons (eql quote)) 'pprint-quasiquote priority table)
  ;; Printing the macro-expansion-time forms
  (set-pprint-dispatch '(cl:cons (eql quasiquote)) 'pprint-unquasiquote priority table)
  (set-pprint-dispatch '(cl:cons (eql unquote)) 'pprint-unquasiquote priority table)
  (set-pprint-dispatch '(cl:cons (eql unquote-splicing)) 'pprint-unquasiquote priority table)
  (set-pprint-dispatch '(cl:cons (eql unquote-nsplicing)) 'pprint-unquasiquote priority table)
  t)

(defvar *fq-pprint-dispatch*
  (let ((table (copy-pprint-dispatch nil)))
    (enable-qq-pp :table table)
    table))
