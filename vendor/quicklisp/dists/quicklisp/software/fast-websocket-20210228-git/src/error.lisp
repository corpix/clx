(in-package :cl-user)
(defpackage fast-websocket.error
  (:use :cl)
  (:import-from :alexandria
                #:plist-hash-table
                #:hash-table-values)
  (:export #:websocket-error
           #:websocket-parse-error
           #:protocol-error
           #:too-large
           #:unacceptable
           #:encoding-error

           #:error-code
           #:error-code-name
           #:valid-error-code-p
           #:acceptable-error-code-p))
(in-package :fast-websocket.error)

(defconstant +min-reserved-error+ 3000)
(defconstant +max-reserved-error+ 4999)

(define-condition websocket-error (error) ())
(define-condition websocket-parse-error (websocket-error) ())

(define-condition protocol-error (websocket-parse-error simple-error) ())

(define-condition too-large (protocol-error)
  ((length :initarg :length)
   (max-length :initarg :max-length))
  (:report
   (lambda (condition stream)
     (with-slots (length max-length) condition
       (format stream "WebSocket frame length too large (~D exceeded the limit ~D)"
               length
               max-length)))))

(define-condition unacceptable (protocol-error)
  ((require-masking :initarg :require-masking))
  (:report
   (lambda (condition stream)
     (format stream  "Recieved ~:[masked~;unmasked~] frame but masking is ~:*~:[not required~;required~]"
             (slot-value condition 'require-masking)))))

(define-condition encoding-error (protocol-error) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Could not decode a text frame as UTF-8"))))

(defparameter *error-codes-map*
  (plist-hash-table '(:normal-closure       1000
                      :going-away           1001
                      :protocol-error       1002
                      :unacceptable         1003
                      :encoding-error       1007
                      :policy-violation     1008
                      :too-large            1009
                      :extension-error      1010
                      :unexpected-condition 1011)
                    :test 'eq))

(defparameter *error-codes-name-map*
  (plist-hash-table '(1000 :normal-closure
                      1001 :going-away
                      1002 :protocol-error
                      1003 :unacceptable
                      1007 :encoding-error
                      1008 :policy-violation
                      1009 :too-large
                      1010 :extension-error
                      1011 :unexpected-condition)
                    :test 'eql))

(defparameter *error-codes*
  (hash-table-values *error-codes-map*))

(defun error-code (error)
  (etypecase error
    (keyword (gethash error *error-codes-map*))
    (too-large      1009)
    (unacceptable   1003)
    (encoding-error 1007)
    (protocol-error 1002)))

(defun error-code-name (code)
  (gethash code *error-codes-name-map*))

(defun valid-error-code-p (code)
  (not (null (error-code-name code))))

(defun acceptable-error-code-p (code)
  (and (integerp code)
       (or (<= +min-reserved-error+ code +max-reserved-error+)
           (valid-error-code-p code))))
