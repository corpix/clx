(in-package #:cl-user)
(defpackage clx/time
  (:use #:clx/std)
  (:local-nicknames (#:lt #:local-time)
		    (#:ltd #:local-time-duration))
  (:export #:unixnano-to-timestamp))
(in-package #:clx/time)

;; timestamp-difference conflicts with same symbol from local-time-duration
;; so exluding it, having difference as duration is better
;; (will reexport local-time-duration:timestamp-difference)
(reexport-from :local-time :exclude '(:timestamp-difference))
(reexport-from :local-time-duration)

;;

(defconstant +nanosecond+ (expt 10 9))

(defun unixnano-to-timestamp (unix)
  (multiple-value-bind (seconds nanoseconds)
      (floor unix +nanosecond+)
    (local-time:unix-to-timestamp seconds :nsec nanoseconds)))
