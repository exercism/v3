;; Ensures that lillys-lasagna-leftovers.lisp and the testing library are always loaded
(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "lillys-lasagna-leftovers")
  (ql:quickload :fiveam))

;; Defines the testing package with symbols from lillys-lasagna-leftovers and FiveAM in scope
;; The `run-tests` function is exported for use by both the user and test-runner
(defpackage :lillys-lasagna-leftovers-test
  (:use :cl :fiveam :lillys-lasagna-leftovers)
  (:export :run-tests))

;; Enter the testing package
(in-package :lillys-lasagna-leftovers-test)

;; Define and enter a new FiveAM test-suite
(def-suite lillys-lasagna-leftovers-suite)
(in-suite lillys-lasagna-leftovers-suite)

;; Either provides human-readable results to the user or machine-readable
;; results to the test runner. The default upon calling `(run-tests)` is to
;; explain the results in a human-readable way
(defun run-tests (&optional (explain t))
  (let ((tests (run 'lillys-lasagna-leftovers-suite))) ; Run the tests once
    (if explain (explain! tests) tests))) ; Optionally explain the results
