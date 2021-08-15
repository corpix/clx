(in-package #:cl-user)
(defpackage clx/std
  (:use #:cl #:cl-reexport))
(in-package #:clx/std)

(reexport-from :cl)
(reexport-from :cl-reexport)
(reexport-from :cl-octet-streams)
(reexport-from :alexandria)
(reexport-from :serapeum)
(reexport-from :arrow-macros :exclude '(:->))
(reexport-from :bordeaux-threads)
(reexport-from :usocket)
