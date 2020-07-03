(load "sameness.lisp")

(ql:quickload :fiveam)
(defpackage sameness-test
  (:use :cl :fiveam :sameness)
  (:export :run-tests))
(in-package :sameness-test)

(def-suite sameness-suite)
(in-suite sameness-suite)

(define-condition explosion () ())
(define-condition sad-trombone () ())
(define-condition victory ())

(defconstant +mazes+ '((maze-1 '((("wrong" . "WRONG") . explosion)
                                 ((2 2.0) . explosion)
                                 ((lisp LISP)) . victory))
                       (maze-2 '(((#\a #\A) . explosion)
                                 ((#\a #\a) . victory)))))

(defun run-maze (maze-id robot)
  (let ((key (robot maze-id))
        (maze (assoc maze-id +mazes+)))
    (loop
       for (door . behind-the-door) in maze
       do (when (apply key door) (signal behind-the-door))
       finally (signal 'sad-trombone))))

(test maze-1-object-equality ""
  (signals 'victory (run-maze 'maze-1 #'robot)))
