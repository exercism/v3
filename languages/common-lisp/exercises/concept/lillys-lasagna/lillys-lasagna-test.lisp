;; Ensures that lillys-lasagna.lisp and the testing library are always loaded
(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "lillys-lasagna")
  (ql:quickload :fiveam))

;; Defines the testing package with symbols from lillys-lasagna and FiveAM in scope
;; The `run-tests` function is exported for use by both the user and test-runner
(defpackage :lillys-lasagna-test
  (:use :cl :fiveam :lillys-lasagna)
  (:export :run-tests))

;; Enter the testing package
(in-package :lillys-lasagna-test)

;; Define and enter a new FiveAM test-suite
(def-suite lillys-lasagna-suite)
(in-suite lillys-lasagna-suite)

(test expected-time "Compute the expected time in the oven"
  (is (= 337 (expected-time-in-oven)))
  (is (equal "Number of minutes Lasagna should be in oven."
             (func-docstring 'expected-time-in-oven))))

(test remaining-time "Compute how many minutes left for cooking"
  (is (= 237 (remaining-minutes-in-oven 100)))
  (is (= 37 (remaining-minutes-in-oven 300)))
  (is (equal "Number of minutes remaining when Lasagna has been in the oven for IN-OVEN minutes."
             (func-docstring 'remaining-minutes-in-oven))))

(test preparation-time "Compute preparation time based upon number of layers"
  (is (= 57 (preparation-time-in-minutes 3)))
  (is (= 76 (preparation-time-in-minutes 4)))
  (is (equal "Number of minutes for preparation of Lasagna with NUM-LAYERS number of layers."
             (func-docstring 'preparation-time-in-minutes))))

(test elapsed-time "Compute sum of prepration time and time lasagna has already been in the oven."
  (is (= 157 (elapsed-time-in-minutes 3 100)))
  (is (= 77 (elapsed-time-in-minutes 4 1)))
  (is (equal "Number of elapsed minutes given NUM-LAYERS on the Lasagna and IN-OVEN minutes it has already been in the oven."
             (func-docstring 'elapsed-time-in-minutes))))

;; Test helper function
(defun func-docstring (func)
  (substitute #\Space #\NewLine (documentation func 'function)))

;; Either provides human-readable results to the user or machine-readable
;; results to the test runner. The default upon calling `(run-tests)` is to
;; explain the results in a human-readable way
(defun run-tests (&optional (explain t))
  (let ((tests (run 'lillys-lasagna-suite))) ; Run the tests once
    (if explain (explain! tests) tests))) ; Optionally explain the results
