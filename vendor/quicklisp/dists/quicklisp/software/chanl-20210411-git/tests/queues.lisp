;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan, Adlai Chandrasekhar
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :chanl)

(def-suite queues :in chanl)
(in-suite queues)

(test fresh-queues
  (is (queue-empty-p (make-queue 5)))
  (is (zerop (queue-count (make-queue 5))))
  (is (not (queue-full-p (make-queue 5))))
  (is (= 5 (queue-length (make-queue 5)))))

(defmacro with-queue-tests ((queue length-form) &body body)
  (let ((naive-queue (gensym)) (naive-tail (gensym)) (count (gensym)) (length (gensym)))
    `(let ((,length ,length-form))
       (let (,naive-queue ,naive-tail (,count 0) (,queue (make-queue ,length)))
         (flet ((test-enqueue ()
                  (let ((item (gensym)))
                    (pushend (enqueue item ,queue) ,naive-queue ,naive-tail)
                    (is (= (incf ,count) (queue-count ,queue)))))
                (test-dequeue ()
                  (is (eq (pop ,naive-queue) (dequeue ,queue)))
                  (is (= (decf ,count) (queue-count ,queue)))))
           (declare (ignore (function test-enqueue) (function test-dequeue)))
           (macrolet ((queue-loop (&body body)
                        `(loop repeat (1+ ,',length) ,@body)))
             ,@body))))))

(test queue-simple
  (with-queue-tests (q 5)
    (test-enqueue)
    (test-enqueue)
    (test-enqueue)
    (test-dequeue)
    (test-dequeue)
    (test-dequeue)))

(test queue-full-p
  (with-queue-tests (q 5)
    (queue-loop never (queue-full-p q) do (test-enqueue))
    (is (= (queue-length q) (queue-count q)))))

(test (queue-empty-p :depends-on queue-full-p)
  (with-queue-tests (q 5)
    (loop until (queue-full-p q) do (test-enqueue))
    (queue-loop never (queue-empty-p q) do (test-dequeue))
    (is (zerop (queue-count q)))))

(test queue-length-error
  (signals queue-length-error (make-queue 0))
  (signals queue-length-error (make-queue -1))
  (signals queue-length-error (make-queue (1+ most-positive-fixnum)))
  (signals queue-length-error (make-queue :P)))

(test queue-overflow-error
  (signals queue-overflow-error
    (with-queue-tests (q 1)
      (queue-loop do (test-enqueue)))))

(test (queue-underflow-error :depends-on queue-overflow-error)
  (signals queue-underflow-error
    (with-queue-tests (q 5)
      (ignore-errors (loop (test-enqueue)))
      (queue-loop do (test-dequeue)))))

(test (queue-stress-test :depends-on queue-underflow-error)
  (with-queue-tests (q 5)
    (queue-loop do
      (signals queue-overflow-error
        (queue-loop do (test-enqueue)))
      (signals queue-underflow-error
        (queue-loop do (test-dequeue))))))
