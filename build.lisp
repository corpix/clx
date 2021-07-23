(ql:quickload :clx/apps/main)
(gc)
(sb-ext:save-lisp-and-die
 #p"main"
 :toplevel #'clx/apps/main:main
 :executable t)
