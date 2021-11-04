;; -*- lexical-binding: t; -*-
(require 'sly-autodoc "contrib/sly-autodoc")
(require 'sly-tests "lib/sly-tests")
(require 'cl-lib)

(defun sly-autodoc-to-string ()
  "Retrieve and return autodoc for form at point."
  (let ((autodoc (car (sly-eval
		       `(slynk:autodoc
			 ',(sly-autodoc--parse-context)
			 :print-right-margin
			 ,(window-width (minibuffer-window)))))))
    (if (eq autodoc :not-available)
        :not-available
        (sly-autodoc--canonicalize-whitespace autodoc))))

(defun sly-check-autodoc-at-point (arglist)
  (sly-test-expect (format "Autodoc in `%s' (at %d) is as expected"
                             (buffer-string) (point))
                     arglist
                     (sly-autodoc-to-string)))

(defmacro define-autodoc-tests (&rest specs)
  `(progn
     ,@(cl-loop
        for (buffer-sexpr wished-arglist . options)
        in specs
        for fails-for = (plist-get options :fails-for)
        for skip-trailing-test-p = (plist-get options :skip-trailing-test-p)
        for i from 1
        when (featurep 'ert)
        collect `(define-sly-ert-test ,(intern (format "autodoc-tests-%d" i))
                   ()
                   ,(format "Check autodoc works ok for %s" buffer-sexpr)
                   ,@(if fails-for
                         `(:expected-result
                           '(satisfies
                             (lambda (result)
                               (ert-test-result-type-p
                                result
                                (if (member (sly-lisp-implementation-name)
                                            ',fails-for)
                                    :failed
                                  :passed))))))
                   (sly-sync-to-top-level 0.3)
                   (sly-check-top-level)
                   (with-temp-buffer
                     (setq sly-buffer-package "COMMON-LISP-USER")
                     (lisp-mode)
                     (insert ,buffer-sexpr)
                     (search-backward "*HERE*")
                     (delete-region (match-beginning 0) (match-end 0))
                     (should (equal ,wished-arglist
                                    (sly-autodoc-to-string)))
                     (unless ,skip-trailing-test-p
                       (insert ")") (backward-char)
                       (should (equal ,wished-arglist
                                      (sly-autodoc-to-string)))))
                   (sly-sync-to-top-level 0.3)))))

(define-autodoc-tests
  ;; Test basics
  ("(slynk::emacs-connected*HERE*"    "(emacs-connected)")
  ("(slynk::emacs-connected *HERE*"   "(emacs-connected)")
  ("(slynk::create-socket*HERE*"
   "(create-socket host port &key backlog)")
  ("(slynk::create-socket *HERE*"
   "(create-socket ===> host <=== port &key backlog)")
  ("(slynk::create-socket foo *HERE*"
   "(create-socket host ===> port <=== &key backlog)")

  ;; Test that autodoc differentiates between exported and
  ;; unexported symbols.
  ("(slynk:create-socket*HERE*" :not-available)

  ;; Test if cursor is on non-existing required parameter
  ("(slynk::create-socket foo bar *HERE*"
   "(create-socket host port &key backlog)")

  ;; Test cursor in front of opening parenthesis
  ("(slynk::with-struct *HERE*(foo. x y) *struct* body1)"
   "(with-struct (conc-name &rest names) obj &body body)"
   :skip-trailing-test-p t)

  ;; Test variable content display
  ("(progn slynk::default-server-port*HERE*"
   "DEFAULT-SERVER-PORT => 4005")

  ;; Test that "variable content display" is not triggered for
  ;; trivial constants.
  ("(slynk::create-socket t*HERE*"
   "(create-socket ===> host <=== port &key backlog)")
  ("(slynk::create-socket :foo*HERE*"
   "(create-socket ===> host <=== port &key backlog)")

  ;; Test with syntactic sugar
  ("#'(lambda () (slynk::create-socket*HERE*"
   "(create-socket host port &key backlog)")
  ("`(lambda () ,(slynk::create-socket*HERE*"
   "(create-socket host port &key backlog)")
  ("(remove-if #'(lambda () (slynk::create-socket*HERE*"
   "(create-socket host port &key backlog)")
  ("`(remove-if #'(lambda () ,@(slynk::create-socket*HERE*"
   "(create-socket host port &key backlog)")

  ;; Test &optional
  ("(slynk::symbol-status foo *HERE*"
   "(symbol-status symbol &optional\
 ===> (package (symbol-package symbol)) <===)" :fails-for ("allegro" "ccl"))

  ;; Test context-sensitive autodoc (DEFMETHOD)
  ("(defmethod slynk::arglist-dispatch (*HERE*"
   "(defmethod arglist-dispatch\
 (===> operator <=== arguments) &body body)")
  ("(defmethod slynk::arglist-dispatch :before (*HERE*"
   "(defmethod arglist-dispatch :before\
 (===> operator <=== arguments) &body body)")

  ;; Test context-sensitive autodoc (APPLY)
  ("(apply 'slynk::eval-for-emacs*HERE*"
   "(apply 'eval-for-emacs &optional form buffer-package id &rest args)")
  ("(apply #'slynk::eval-for-emacs*HERE*"
   "(apply #'eval-for-emacs &optional form buffer-package id &rest args)" :fails-for ("ccl"))
  ("(apply 'slynk::eval-for-emacs foo *HERE*"
   "(apply 'eval-for-emacs &optional form\
 ===> buffer-package <=== id &rest args)")
  ("(apply #'slynk::eval-for-emacs foo *HERE*"
   "(apply #'eval-for-emacs &optional form\
 ===> buffer-package <=== id &rest args)" :fails-for ("ccl"))

  ;; Test context-sensitive autodoc (ERROR, CERROR)
  ("(error 'simple-condition*HERE*"
   "(error 'simple-condition &rest arguments\
 &key format-arguments format-control)" :fails-for ("ccl"))
  ("(cerror \"Foo\" 'simple-condition*HERE*"
   "(cerror \"Foo\" 'simple-condition\
 &rest arguments &key format-arguments format-control)"
   :fails-for ("ccl"))

  ;; Test &KEY and nested arglists
  ("(slynk::with-retry-restart (:msg *HERE*"
   "(with-retry-restart (&key ===> (msg \"Retry.\") <===) &body body)"
   :fails-for ("allegro"))
  ("(slynk::with-retry-restart (:msg *HERE*(foo"
   "(with-retry-restart (&key ===> (msg \"Retry.\") <===) &body body)"
   :skip-trailing-test-p t
   :fails-for ("allegro"))
  ("(slynk::start-server \"/tmp/foo\" :dont-close *HERE*"
   "(start-server port-file &key (style slynk:*communication-style*)\
 ===> (dont-close slynk:*dont-close*) <===)"
   :fails-for ("allegro" "ccl"))

  ;; Test declarations and type specifiers
  ("(declare (string *HERE*"
   "(declare (string &rest ===> variables <===))"
   :fails-for ("allegro") :fails-for ("ccl"))
  ("(declare ((string *HERE*"
   "(declare ((string &optional ===> size <===) &rest variables))")
  ("(declare (type (string *HERE*"
   "(declare (type (string &optional ===> size <===) &rest variables))")

  ;; Test local functions
  ("(flet ((foo (x y) (+ x y))) (foo *HERE*" "(foo ===> x <=== y)")
  ("(macrolet ((foo (x y) `(+ ,x ,y))) (foo *HERE*" "(foo ===> x <=== y)")
  ("(labels ((foo (x y) (+ x y))) (foo *HERE*" "(foo ===> x <=== y)")
  ("(labels ((foo (x y) (+ x y))
                 (bar (y) (foo *HERE*"
   "(foo ===> x <=== y)" :fails-for ("cmucl" "sbcl" "allegro" "ccl")))

(def-sly-test autodoc-space
    (input-keys expected-message)
    "Emulate the inserting something followed by the space key
event and verify that the right thing appears in the echo
area (after a short delay)."
    '(("( s l y n k : : o p e r a t o r - a r g l i s t SPC"
       "(operator-arglist name package)"))
  (when noninteractive
    (sly-skip-test "Can't use unread-command-events in batch mode"))
  (let* ((keys (eval `(kbd ,input-keys)))
	 (tag (cons nil nil))
	 (timerfun (lambda (tag) (throw tag nil)))
	 (timer (run-with-timer 1 nil timerfun tag)))
    (with-temp-buffer
      (lisp-mode)
      (unwind-protect
	  (catch tag
	    (message nil)
	    (select-window (display-buffer (current-buffer) t))
	    (setq unread-command-events (listify-key-sequence keys))
	    (accept-process-output)
	    (recursive-edit))
	(setq unread-command-events nil)
	(cancel-timer timer))
      (sly-test-expect "Message after SPC"
			 expected-message (current-message))
      (accept-process-output nil (* eldoc-idle-delay 2))
      (sly-test-expect "Message after edloc delay"
			 expected-message (current-message)))))

(provide 'sly-autodoc-tests)
