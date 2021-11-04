;;;; package.lisp

(defpackage #:cl-package-locks
  (:use #:cl)
  (:export :resolve-package
	   :resolve-packages
	   :package-locked-p
	   :locked-packages
	   :all-locked-packages
	   :unlocked-packages
	   :all-unlocked-packages
	   :lock-package
	   :lock-packages
	   :unlock-package
	   :unlock-packages
	   :with-packages-unlocked
	   :without-package-locks))

