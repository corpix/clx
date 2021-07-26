(ql:quickload :clx/apps/main)

(defun main ()
  ;; XXX: is this the right way to put some metadata into binary?
  (let ((clx/apps/main:*version* "1.0"))
    (clx/apps/main:main)))

(sb-ext:save-lisp-and-die
 #p"main"
 :toplevel #'main
 :purify t
 ;; FIXME: rebuild sbcl with zlib
 ;;:compression t
 :executable t)
