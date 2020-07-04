;; Ensures that sameness.lisp and the testing library are always loaded
(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "sameness")
  (ql:quickload :fiveam))

;; Defines the testing package with symbols from sameness and FiveAM in scope
;; The `run-tests` function is exported for use by both the user and test-runner
(defpackage sameness-test
  (:use :cl :fiveam :sameness)
  (:export :run-tests))

;; Enter the testing package
(in-package :sameness-test)

;; Define and enter a new FiveAM test-suite
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


;; Either provides human-readable results to the user or machine-readable
;; results to the test runner. The default upon calling `(run-tests)` is to
;; explain the results in a human-readable way
(defun run-tests (&optional (explain t))
  (let ((tests (run 'sameness-suite))) ; Run the tests once
    (if explain (explain! tests) tests))) ; Optionally explain the results
