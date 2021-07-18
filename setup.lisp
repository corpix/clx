(require "asdf")
(push (car (directory ".")) asdf:*central-registry*)
(ql:quickload :clx)
