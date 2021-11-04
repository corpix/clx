;; -*- lexical-binding: t; -*-
(require 'sly-fontifying-fu "contrib/sly-fontifying-fu")
(require 'sly-tests "lib/sly-tests")
(require 'sly-autodoc "contrib/sly-autodoc")

(cl-defun sly-initialize-lisp-buffer-for-test-suite
    (&key (font-lock-magic t) (autodoc t))
  (let ((hook lisp-mode-hook))
    (unwind-protect
        (progn
          (set (make-local-variable 'sly-highlight-suppressed-forms)
               font-lock-magic)
          (setq lisp-mode-hook nil)
          (lisp-mode)
          (sly-mode 1)
          (when (boundp 'sly-autodoc-mode)
            (if autodoc
                (sly-autodoc-mode 1)
              (sly-autodoc-mode -1))))
      (setq lisp-mode-hook hook))))

(def-sly-test font-lock-magic (buffer-content)
    "Some testing for the font-lock-magic. *YES* should be
    highlighted as a suppressed form, *NO* should not."

    '(("(defun *NO* (x y) (+ x y))")
      ("(defun *NO*")
      ("*NO*) #-(and) (*YES*) (*NO* *NO*")
      ("\(
\(defun *NO*")
      ("\)
\(defun *NO*
    \(
\)")
      ("#+#.foo
\(defun *NO* (x y) (+ x y))")
      ("#+#.foo
\(defun *NO* (x ")
      ("#+(
\(defun *NO* (x ")
      ("#+(test)
\(defun *NO* (x ")

      ("(eval-when (...)
\(defun *NO* (x ")

      ("(eval-when (...)
#+(and)
\(defun *NO* (x ")

      ("#-(and) (defun *YES* (x y) (+ x y))")
      ("
#-(and) (defun *YES* (x y) (+ x y))
#+(and) (defun *NO* (x y) (+ x y))")

      ("#+(and) (defun *NO* (x y) #-(and) (+ *YES* y))")
      ("#| #+(or) |# *NO*")
      ("#| #+(or) x |# *NO*")
      ("*NO* \"#| *NO* #+(or) x |# *NO*\" *NO*")
      ("#+#.foo (defun foo (bar))
#-(and) *YES* *NO* bar
")
      ("#+(foo) (defun foo (bar))
#-(and) *YES* *NO* bar")
      ("#| #+(or) |# *NO* foo
#-(and) *YES* *NO*")
      ("#- (and)
\(*YES*)
\(*NO*)
#-(and)
\(*YES*)
\(*NO*)")
      ("#+nil (foo)

#-(and)
#+nil (
       asdf *YES* a
            fsdfad)

\( asdf *YES*

       )
\(*NO*)

")
      ("*NO*

#-(and) \(progn
   #-(and)
   (defun *YES* ...)

   #+(and)
   (defun *YES* ...)

   (defun *YES* ...)

   *YES*

   *YES*

   *YES*

   *YES*
\)

*NO*")
      ("#-(not) *YES* *NO*

*NO*

#+(not) *NO* *NO*

*NO*

#+(not a b c) *NO* *NO*

*NO*"))
  (sly-check-top-level)
  (with-temp-buffer
    (insert buffer-content)
    (sly-initialize-lisp-buffer-for-test-suite
     :autodoc t :font-lock-magic t)
    ;; Can't use `font-lock-fontify-buffer' because for the case when
    ;; `jit-lock-mode' is enabled. Jit-lock-mode fontifies only on
    ;; actual display.
    (font-lock-default-fontify-buffer)
    (when (search-backward "*NO*" nil t)
      (sly-test-expect "Not suppressed by reader conditional?"
                         'sly-reader-conditional-face
                         (get-text-property (point) 'face)
                         #'(lambda (x y) (not (eq x y)))))
    (goto-char (point-max))
    (when (search-backward "*YES*" nil t)
      (sly-test-expect "Suppressed by reader conditional?"
                         'sly-reader-conditional-face
                         (get-text-property (point) 'face)))))

(provide 'sly-fontifying-fu-tests)
