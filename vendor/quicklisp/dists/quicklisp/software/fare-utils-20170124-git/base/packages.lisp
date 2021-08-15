;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Basic Utilities about Packages

#+xcvb (module (:depends-on ("package" "base/utils" "base/macros")))

(in-package :fare-utils)

(exporting-definitions

(defun make-defpackage-form (package-designator &optional (style :string))
  (let* ((p (find-package package-designator))
         (name (package-name p))
         (nicknames (sort (package-nicknames p)
                          #'string<))
         (documentation (documentation p t))
         (use (package-use-list p))
         (shadow nil)
         (shadowing-import-from (make-hash-table :test 'equal))
         (import-from (make-hash-table :test 'equal))
         (export nil)
         ;;(intern nil)
         )
    (labels
        ((s (x)
           (ecase style
             (:keyword (conc-keyword x))
             (:gensym (make-symbol x))
             (:string x)))
         (ss (strings)
           (mapcar #'s strings))
         (sss (strings)
           (ss (sort strings #'string<)))
         (sl (symbols)
           (sss (mapcar #'symbol-name symbols)))
         (pl (packages)
           (ss (mapcar #'package-name packages)))
         (w1 (x y) (when y `((,x ,y))))
         (w* (x y) (when y `((,x ,@y))))
         (w** (x h)
           (sort (loop :for package :being :the :hash-keys :of h :using (:hash-value symbols)
                   :collect (list* x (s (package-name package)) (sl symbols)))
                 #'string<
                 :key #'(lambda (x) (symbol-name (second x)))))
         (fs (name pl)
           (loop :for p :in pl :thereis (find-symbol name p)))
         (sx (sym)
           (let ((name (symbol-name sym))
                 (home (symbol-package sym)))
             (if (eq home p)
               (when (fs name use)
                   (pushnew sym shadow))
               (if (fs name use)
                   (unless (loop :for p :in use :always (eq sym (find-symbol name p)))
                     (pushnew sym (gethash home shadowing-import-from)))
                       (pushnew sym (gethash home import-from)))))))
      (do-symbols (sym p)
        (multiple-value-bind (symbol status) (find-symbol (symbol-name sym) p)
          (unless (eq sym symbol) (error "symbol mismatch"))
          (ecase status
            (:external
             (pushnew sym export)
             (sx sym))
            (:internal
             (or (sx sym)
                 ;;(pushnew sym intern)
                 ))
            (:inherited nil))))
      (let* ((x (loop :for key :being :the :hash-keys :of (package-exported-symbols p)
                      :collect key))
             (xnd (set-difference x export))
             (dnx (set-difference export x))
             (*print-level* nil)
             (*print-length* nil))
        (when xnd
          (warn "Current defpackage for ~A doesn't include declared exports~% ~S" (package-name p) xnd))
        (when dnx
          (warn "Current defpackage for ~A includes undeclared exports~% ~S" (package-name p) dnx)))
    `(defpackage
      ,(s name)
      ,@(w* :nicknames nicknames)
      ,@(w1 :documentation documentation)
      ,@(w* :use (pl use))
      ,@(w* :shadow (sl shadow))
      ,@(w** :shadowing-import-from shadowing-import-from)
      ,@(w** :import-from import-from)
      ,@(w* :export (sl export))
      ;;,@(w* :intern (sl intern))
      ))))

(defmacro define-package-mix (package mixed-packages &rest clauses)
  (let ((h (make-hash-table :test 'equal)))
    (labels ((ensure-imported (n)
               (let* ((s (string n))
                      (x (gethash s h)))
                 (unless x (setf (gethash s h) t))
                 x))
             (import-from (package)
               (loop :for s :being :each :external-symbol :in package
                 :for n = (symbol-name s)
                 :unless (ensure-imported n)
                 :collect n)))
      ;; First, mark the symbols explicitly imported by the user
      (loop :for (kw . ()) :in clauses
        :when (member kw '(:import-from :shadowing-import-from)) :do
        (map () #'ensure-imported (cddr clauses)))
      `(defpackage ,package (:use)
         ,@(loop :for p :in mixed-packages
             :collect `(:import-from ,p ,@(import-from p)))
         ,@clauses
         (:export ,@(loop :for s :being :the :hash-keys :of h :collect s)))))))
