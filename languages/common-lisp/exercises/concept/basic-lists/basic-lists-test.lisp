;; Ensures that basic-lists.lisp and the testing library are always loaded
(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "basic-lists")
  (ql:quickload :fiveam))

;; Defines the testing package with symbols from basic-lists and FiveAM in scope
;; The `run-tests` function is exported for use by both the user and test-runner
(defpackage basic-lists-test
  (:use :cl :fiveam :basic-lists)
  (:export :run-tests))

;; Enter the testing package
(in-package :basic-lists-test)

;; Define and enter a new FiveAM test-suite
(def-suite basic-lists-suite)
(in-suite basic-lists-suite)

;; Either provides human-readable results to the user or machine-readable
;; results to the test runner. The default upon calling `(run-tests)` is to
;; explain the results in a human-readable way
(defun run-tests (&optional (explain t))
  (let ((tests (run 'basic-lists-suite))) ; Run the tests once
    (if explain (explain! tests) tests))) ; Optionally explain the results
