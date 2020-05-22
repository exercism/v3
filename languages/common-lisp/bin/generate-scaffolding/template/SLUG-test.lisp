;; Ensures that SLUG.lisp and the testing library are always loaded
(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "SLUG")
  (ql:quickload :fiveam))

;; Defines the testing package with symbols from SLUG and FiveAM in scope
;; The `run-tests` function is exported for use by both the user and test-runner
(defpackage SLUG-test
  (:use :cl :fiveam :SLUG)
  (:export :run-tests))

;; Enter the testing package
(in-package :SLUG-test)

;; Define and enter a new FiveAM test-suite
(def-suite SLUG-suite)
(in-suite SLUG-suite)

;; Either provides human-readable results to the user or machine-readable
;; results to the test runner. The default upon calling `(run-tests)` is to
;; explain the results in a human-readable way
(defun run-tests (&optional (explain t))
  (let ((tests (run 'SLUG-suite))) ; Run the tests once
    (if explain (explain! tests) tests))) ; Optionally explain the results
