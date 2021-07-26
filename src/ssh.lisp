(in-package :cl-user)
(defpackage clx/ssh
  (:nicknames :clx/ssh)
  (:use :cl :uiop))
(in-package :clx/ssh)

(defparameter *ssh* (launch-program '("ssh" "rab.backbone")
				    :input :stream :output :stream))

(process-alive-p *ssh*)
(wait-process *ssh*)
(slot-value *ssh* 'uiop/launch-program::exit-code)

(write-line "ip a" (process-info-input *ssh*))
(force-output (process-info-input *ssh*))

(read-line (process-info-output *ssh*))
(close-streams *ssh*)

(defun with-ssh ()
  )
