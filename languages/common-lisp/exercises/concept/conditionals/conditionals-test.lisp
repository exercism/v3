;; Ensures that conditionals.lisp and the testing library are always loaded
(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "conditionals")
  (ql:quickload :fiveam))

;; Defines the testing package with symbols from conditionals and FiveAM in scope
;; The `run-tests` function is exported for use by both the user and test-runner
(defpackage conditionals-test
  (:use :cl :fiveam :conditionals)
  (:export :run-tests))

;; Enter the testing package
(in-package :conditionals-test)

;; Define and enter a new FiveAM test-suite
(def-suite conditionals-suite)
(in-suite conditionals-suite)

;; Either provides human-readable results to the user or machine-readable
;; results to the test runner. The default upon calling `(run-tests)` is to
;; explain the results in a human-readable way
(defun run-tests (&optional (explain t))
  (let ((tests (run 'conditionals-suite))) ; Run the tests once
    (if explain (explain! tests) tests))) ; Optionally explain the results
