(defpackage :local-time-duration-tests-system (:use #:asdf #:cl))
(in-package :local-time-duration-tests-system)

(defsystem :local-time-duration-tests
    :depends-on (:local-time-duration :fiveam)
    :serial t
    :components
    ((:file "package")
     (:file "duration")
     (:file "timestamp")))
