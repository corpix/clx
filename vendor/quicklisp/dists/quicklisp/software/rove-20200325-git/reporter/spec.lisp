(in-package #:cl-user)
(defpackage #:rove/reporter/spec
  (:use #:cl
        #:rove/core/stats
        #:rove/core/result
        #:rove/reporter
        #:rove/misc/stream
        #:rove/misc/color
        #:rove/utils/reporter)
  (:import-from #:dissect
                #:present-object)
  (:export #:spec-reporter))
(in-package #:rove/reporter/spec)

(defclass spec-reporter (reporter) ())

(defmethod initialize-instance :after ((reporter spec-reporter) &rest initargs &key stream &allow-other-keys)
  (declare (ignore initargs))
  (when stream
    (setf (reporter-stream reporter)
          (make-indent-stream stream))))

(defun print-duration (duration stream)
  (when duration
    (let ((color (cond
                   ((< 75 duration) :red)
                   ((< (/ 75 2) duration) :yellow))))
      (when color
        (princ
         (color-text color (format nil " (~Dms)" duration))
         stream)))))

(defmethod record ((reporter spec-reporter) (object passed-assertion))
  (call-next-method)
  (let ((stream (reporter-stream reporter)))
    (fresh-line stream)
    (princ (color-text :green "✓ ") stream)
    (with-indent (stream +2)
      (princ (color-text :gray (assertion-description object)) stream)
      (print-duration (assertion-duration object) stream)
      (fresh-line stream))))

(defmethod record ((reporter spec-reporter) (object failed-assertion))
  (call-next-method)
  (let ((stream (reporter-stream reporter)))
    (fresh-line stream)
    (princ (color-text :red "× ") stream)
    (with-indent (stream +2)
      (princ
       (color-text :red
                   (format nil "~D) ~A"
                           (1- (length (all-failed-assertions *stats*)))
                           (assertion-description object)))
       stream)
      (print-duration (assertion-duration object) stream)
      (fresh-line stream))))

(defmethod record ((reporter spec-reporter) (object pending-assertion))
  (call-next-method)
  (let ((stream (reporter-stream reporter)))
    (fresh-line stream)
    (princ (color-text :aqua "- ") stream)
    (with-indent (stream +2)
      (princ (color-text :aqua (assertion-description object)) stream)
      (fresh-line stream))))

(defmethod test-begin ((reporter spec-reporter) test-name &optional count)
  (declare (ignore count))
  (call-next-method)
  (let ((stream (reporter-stream reporter)))
    (fresh-line stream)
    (when test-name
      (princ (color-text :white test-name) stream)
      (fresh-line stream)
      (incf (stream-indent-level stream) 2))))

(defmethod test-finish ((reporter spec-reporter) test-name)
  (multiple-value-bind (passedp context) (call-next-method)
    (let ((stream (reporter-stream reporter))
          (test-count (context-test-count context)))
      (when test-name
        (decf (stream-indent-level stream) 2))
      (when (toplevel-stats-p reporter)
        (format-failure-tests stream context))
      (when (and (stats-plan context)
                 (/= (stats-plan context) test-count))
        (princ
         (color-text :red
                     (format nil "× Looks like you planned ~D test~:*~P but ran ~A."
                             (stats-plan context)
                             test-count))
         stream)
        (fresh-line stream))
      passedp)))
