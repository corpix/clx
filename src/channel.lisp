(in-package #:cl-user)
(defpackage clx/channel
  (:use #:clx/std)
  (:export #:poll))
(in-package #:clx/channel)

(reexport-from :chanl :include
	       '(:abstract-channel
		 :cas-channel
		 :channel
		 :buffered-channel
		 :stack-channel
		 :queue-channel
		 :bounded-channel
		 :unbounded-channel

		 :channelp
		 :send
		 :recv

		 :send-blocks-p
		 :recv-blocks-p
		 :channel-insert-value
		 :channel-grab-value))

;;

(defmacro poll (&body body)
  "Non-deterministically select a non-blocking clause to execute using CHANL:SELECT"
  `(chanl:select ,@body))
