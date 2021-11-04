(defpackage :serapeum/contrib/hooks
  (:use :common-lisp)
  (:import-from :serapeum
                #:*hook*
                #:with-hook-restart
                #:add-hook
                #:remove-hook
                #:run-hooks
                #:run-hook
                #:run-hook
                #:run-hook-until-failure
                #:run-hook-until-success)
  (:import-from #:alexandria
                #:required-argument)
  (:export
   #:*hook*
   #:add-hook
   #:remove-hook
   #:run-hooks
   #:run-hook
   #:run-hook-with-args-until-failure
   #:run-hook-with-args-until-success
   ;; #:make-handler ; Leave unexported to incite using typed handlers.
   #:default-combine-hook
   #:combine-hook-until-failure
   #:combine-hook-until-success
   #:combine-composed-hook
   #:find-handler
   #:disable-hook
   #:enable-hook
   #:define-hook
   #:find-hook
   #:define-hook-type
   ;; Handler class:
   #:handler
   #:name
   #:fn
   #:description
   #:handler-type
   #:place
   #:value
   ;; Hook class:
   #:hook
   #:handlers
   #:disabled-handlers
   #:combination
   ;; Pre-generated types:
   #:make-handler-void
   #:handler-void
   #:hook-void
   #:make-hook-void
   #:make-handler-string->string
   #:handler-string->string
   #:hook-string->string
   #:make-hook-string->string
   #:make-handler-number->number
   #:handler-number->number
   #:hook-number->number
   #:make-hook-number->number
   #:make-handler-any
   #:handler-any
   #:hook-any
   #:make-hook-any))
(in-package :serapeum/contrib/hooks)

(defclass handler ()
  ((name :initarg :name
         :accessor name
         :type symbol
         :initform nil
         :documentation "
Name of the handler.
If defaults to the function name if `fn' is a named function.
This is useful so that the user can build handlers out of anonymous functions.")
   (description :initarg :description
                :accessor description
                :type string
                :initform ""
                :documentation "
Description of the handler.  This is purely informative.")
   (fn :initarg :fn
     :accessor fn
     :type function
     :initform (required-argument 'fn)
     :documentation "
The handler function.  It can be an anonymous function.")
   (handler-type :initarg :handler-type
                 :accessor handler-type
                 :type t
                 :initform nil
                 :documentation "The function type of FN.
This is purely informative.")
   (place :initarg :place
          :accessor place
          :type (or symbol list)
          :initform nil
          :documentation "
If the handler is meant to be a setter, PLACE describes what is set.
PLACE can be a symbol or a pair (CLASS SLOT).
This can be left empty if the handler is not a setter.")
   (value :initarg :value
          :accessor value
          :type t
          :initform nil
          :documentation "
If the handler is meant to be a setter, VALUE can be used to describe what FN is
going to set to PLACE.
In particular, PLACE and VALUE can be used to compare handlers.
This can be left empty if the handler is not a setter."))
  (:documentation "Handlers are wrappers around functions used in typed hooks.
They serve two purposes as opposed to regular functions:

- They can embed a NAME so that anonymous functions can be conveniently used in lambda.
- If the handler is meant to be a setter, the PLACE and VALUE slot can be used to identify and compare setters.

With this extra information, it's possible to compare handlers and, in particular, avoid duplicates in hooks."))

(defun make-handler (fn &key (class-name 'handler) name handler-type place value)
  "Return a `handler'.
NAME is only mandatory if FN is a lambda.
CLASS-NAME can be used to create handler subclasses.

HANDLER-TYPE, PLACE and VALUE are as per the sltos in `handler-type'.

This function should not be used directly.
Prefer the typed make-handler-* functions instead."
  (setf name (or name (let ((fname (nth-value 2 (function-lambda-expression fn))))
                        (when (typep fname 'symbol)
                          fname))))
  (unless name
    (error "Can't make a handler without a name"))
  (make-instance class-name
                 :name name
                 :fn fn
                 :handler-type handler-type
                 :place place
                 :value value))

(defmethod equals ((fn1 handler) (fn2 handler))
  "Return non-nil if FN1 and FN2 are equal.
Handlers are equal if they are setters of the same place and same value, or if
their names are equal."
  (cond
    ((or (and (place fn1)
              (not (place fn2) ))
         (and (place fn2)
              (not (place fn1) )))
     nil)
    ((and (place fn1)
          (place fn2))
     (and (equal (place fn1)
                 (place fn2))
          (equal (value fn1)
                 (value fn2))))
    (t
     (eq (name fn1)
         (name fn2)))))

(defclass hook ()
  ((handler-class :reader handler-class ; TODO: Is this really needed?
                  :type symbol
                  :initform t
                  :documentation "
The class of the supported handlers.")
   (handlers :initarg :handlers
             :accessor handlers
             :type list
             :initform '()
             :documentation "")
   (disabled-handlers :initarg :disabled-handlers
                      :accessor disabled-handlers
                      :type list
                      :initform '()
                      :documentation "Those handlers are not run by `run-hook'.
This is useful it the user wishes to disable some or all handlers without
removing them from the hook.")
   (combination :initarg :combination
                :accessor combination
                :type function
                :initform #'default-combine-hook
                :documentation "
This can be used to reverse the execution order, return a single value, etc."))
  (:documentation "This hook class serves as support for typed-hook.

Typing in hook is crucial to guarantee that a hook is well formed, i.e. that
it's handlers accept the right argument types and return the right value types.

Because typing is too limited in Common Lisp, we leverage CLOS to generate
subclasses of `hook' and `handlers' with typed helper functions.
The `add-hook' will thus only accept handlers of the right types.
This implementation is good enough to catch typing errors at compile time."))

(defmethod default-combine-hook ((hook hook) &rest args)
  "Return the list of the results of the HOOK handlers applied from youngest to
oldest to ARGS.
Return '() when there is no handler.
This is an acceptable `combination' for `hook'."
  (mapcar (lambda (handler)
            (with-hook-restart (apply (fn handler) args)))
          (handlers hook)))

(defmethod combine-hook-until-failure ((hook hook) &rest args)
  "Return the list of values until the first nil result.
Handlers after the failing one are not run.

You need to check if the hook has handlers to know if a NIL return value is due
to the first handler failing or an empty hook.

This is an acceptable `combination' for `hook'."
  (let ((result nil))
    (loop for handler in (handlers hook)
          for res = (apply (fn handler) args)
          do (push res result)
          always res)
    (nreverse result)))

(defmethod combine-hook-until-success ((hook hook) &rest args)
  "Return the value of the first non-nil result.
Handlers after the successful one are not run.

You need to check if the hook has handlers to know if a NIL return value is due
to all handlers failing or an empty hook.

This is an acceptable `combination' for `hook'."
  (loop for handler in (handlers hook)
          thereis (apply (fn handler) args)))

(defmethod combine-composed-hook ((hook hook) &rest args)
  "Return the result of the composition of the HOOK handlers on ARGS, from
oldest to youngest.
Without handler, return ARGS as values.
This is an acceptable `combination' for `hook'."
  (let ((fn-list (mapcar #'fn (handlers hook))))
    (if fn-list
        (apply (apply #'alexandria:compose fn-list) args)
        (values-list args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun add-hook-internal (hook handler &key append)
  "Add HANDLER to HOOK if not already in it.
Return HOOK.
HANDLER is also not added if in the `disabled-handlers'.
If APPEND is non-nil, HANDLER is added at the end."
  (serapeum:synchronized (hook)
    (unless (or (member handler (handlers hook) :test #'equals)
                (member handler (disabled-handlers hook) :test #'equals))
      (if append
          (alexandria:appendf (symbol-value hook) (list handler))
          (pushnew handler (handlers hook) :test #'equals)))
    hook))

(declaim (ftype (function ((or handler symbol) list) (or handler boolean)) find-handler))
(defun find-handler (handler-or-name handlers)
  "Return handler matching HANDLER-OR-NAME in HANDLERS sequence."
  (apply #'find handler-or-name handlers
         (if (typep handler-or-name 'handler)
             (list :test #'equals)
             (list :key #'name))))

(defun delete* (item sequence
                &key from-end (test #'eql) (start 0)
                     end count key)
  "Like `delete' but return (values NEW-SEQUENCE FOUNDP)
with FOUNDP being T if the element was found, NIL otherwise."
  (let* ((foundp nil)
         (new-sequence (delete-if (lambda (f)
                                    (when (funcall test item f)
                                      (setf foundp t)
                                      t))
                                  sequence
                                  :from-end from-end
                                  :start start
                                  :key key
                                  :end end
                                  :count count)))
    (values new-sequence foundp)))

(defmethod remove-hook ((hook hook) handler-or-name)
  "Remove handler matching HANDLER-OR-NAME from the handlers or the
disabled-handlers in HOOK.
HANDLER-OR-NAME is either a handler object or a symbol.
Return HOOK's handlers."
  (serapeum:synchronized (hook)
    (multiple-value-bind (new-sequence foundp)
        (apply #'delete* handler-or-name (handlers hook)
               (if (typep handler-or-name 'handler)
                   (list :test #'equals)
                   (list :key #'name)))
      (if foundp
          (setf (handlers hook) new-sequence)
          (multiple-value-bind (new-sequence foundp)
              (apply #'delete* handler-or-name (disabled-handlers hook)
                     (if (typep handler-or-name 'handler)
                         (list :test #'equals)
                         (list :key #'name)))
            (when foundp
              (setf (disabled-handlers hook) new-sequence)))))
    (handlers hook)))

(defmethod run-hook ((hook hook) &rest args)
  (apply (combination hook) hook args))

(defmethod run-hook-with-args-until-failure ((hook hook) &rest args)
  "This is equivalent to setting the combination function to
`combine-hook-until-failure' and calling `run-hook'."
  (apply #'combine-hook-until-failure hook args))

(defmethod run-hook-with-args-until-success ((hook hook) &rest args)
  "This is equivalent to setting the combination function to
`combine-hook-until-success' and calling `run-hook'."
  (apply #'combine-hook-until-success hook args))

(defun move-hook-handlers (hook source-handlers-slot destination-handlers-slot select-handlers)
  (serapeum:synchronized (hook)
    (let* ((handlers-to-move (if select-handlers
                                 (intersection (slot-value hook source-handlers-slot)
                                               select-handlers
                                               :test (lambda (e1 e2)
                                                       (if (typep e2 'handler)
                                                           (equals e1 e2)
                                                           (eq (name e1) e2))))
                                 (slot-value hook source-handlers-slot)))
           (handlers-to-keep (set-difference (slot-value hook source-handlers-slot)
                                             handlers-to-move)))
      (setf (slot-value hook destination-handlers-slot)
            (append handlers-to-move (slot-value hook destination-handlers-slot)))
      (setf (slot-value hook source-handlers-slot) handlers-to-keep))))

(defmethod disable-hook ((hook hook) &rest handlers)
  "Prepend HANDLERS to the list of disabled handlers.
Without HANDLERS, disable all of them."
  (move-hook-handlers hook 'handlers 'disabled-handlers handlers))

(defmethod enable-hook ((hook hook) &rest handlers)
  "Prepend HANDLERS to the list of HOOK's handlers.
Without HANDLERS, enable all of them."
  (move-hook-handlers hook 'disabled-handlers 'handlers handlers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global hooks.

;; TODO: cl-hooks uses symbol properties.  Is it a good idea?
(defvar %hook-table (make-hash-table :test #'equal)
  "Global hook table.")

(defun define-hook (hook-type name &key object handlers disabled-handlers combination)
  "Return a globally-accessible hook.
The hook can be accessed with `find-hook' at (list NAME OBJECT)."
  (let ((hook
          (apply #'make-instance hook-type
                 :handlers handlers
                 :disabled-handlers disabled-handlers
                 (if combination
                     (list :combination combination)
                     '()))))
    (setf (gethash (list name object) %hook-table)
          hook)
    hook))

(defun find-hook (name &optional object)
  "Return the global hook with name NAME associated to OBJECT, if provided.
The following examples return different hooks:
- (find-hook 'foo-hook)
- (find-hook 'foo-hook 'bar-class)
- (find-hook 'foo-hook (make-instance 'bar-class))"
  (gethash (list name object) %hook-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compile-time type checking for add-hook is useful in case `add-hook' is not
;; run when the user config is loaded.  Sadly Common Lisp does not seem to allow
;; to extract the type of a function so that it can be checked against what a
;; hook would allow.  We work around this issue with a macro:
;;
;; - Generates hook and handler subclasses.
;; - Generate a handler maker and declaim provided function type.
;; - Generate a `add-hook' defmethod against those 2 classes.

(defmacro define-hook-type (name type)
  "Define hook class and constructor and the associated handler class.
Type must be something like:

  (function (string) (values integer t))

A function with name make-handler-NAME will be created.
A class with name handler-NAME will be created.
The method `add-hook' is added for the new hook and handler types.

The function make-hook-NAME is created.  It is similar to (make-instance
'hook-NAME ...) except that named functions are also accepted.  Named functions
will be automatically encapsulated with make-handler-NAME."
  (let* ((name (string name))
         (function-name (intern (serapeum:concat "MAKE-HANDLER-" name)))
         (handler-class-name (intern (serapeum:concat "HANDLER-" name)))
         (hook-class-name (intern (serapeum:concat "HOOK-" name)))
         (hook-function-name (intern (serapeum:concat "MAKE-HOOK-" name))))
    `(progn
       (defclass ,handler-class-name (handler) ())
       (declaim (ftype (function (,type &key (:name symbol) (:place t) (:value t)))
                       ,function-name))
       (defun ,function-name (fn &key name place value)
         (make-handler fn :class-name ',handler-class-name
                          :name name
                          :handler-type ',type
                          :place place
                          :value value))
       (defclass ,hook-class-name (hook)
         ((handler-class :initform ',handler-class-name)))
       (defmethod add-hook ((hook ,hook-class-name) (handler ,handler-class-name) &key append)
         ,(format nil "Add HANDLER to HOOK.  Return HOOK.
HOOK must be of type ~a
HANDLER must be of type ~a."
                  hook-class-name
                  handler-class-name)
         (add-hook-internal hook handler :append append))
       (defun ,hook-function-name (&key handlers (combination #'default-combine-hook explicit?))
         ,(format nil "Make hook and return it.
HANDLERS can also contain named functions.
Those will automatically be encapsulated with ~a." function-name)
         (when (and explicit?
                    (not (functionp combination)))
           (error "Function combination required."))
         (make-instance ',hook-class-name
                        :handlers (mapcar (lambda (f) (if (typep f 'handler)
                                                          f
                                                          (,function-name f)))
                                          handlers)
                        :combination combination)))))

;; TODO: Allow listing all the hooks?

(define-hook-type void (function ()))
(define-hook-type string->string (function (string) string))
(define-hook-type number->number (function (number) number))
(define-hook-type any (function (&rest t)))
