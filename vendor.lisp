(load #p"vendor/quicklisp-bootstrap/quicklisp.lisp")

(let ((quicklisp-root #p"vendor/quicklisp"))
  (when (not (probe-file quicklisp-root))
    (quicklisp-quickstart:install :path quicklisp-root)))

(quit)
