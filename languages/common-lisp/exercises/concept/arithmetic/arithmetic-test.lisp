;; Ensures that arithmetic.lisp and the testing library are always loaded
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;(load "arithmetic")
  (load ".meta/example")
  (ql:quickload :fiveam))

;; Defines the testing package with symbols from arithmetic and FiveAM in scope
;; The `run-tests` function is exported for use by both the user and test-runner
(defpackage arithmetic-test
  (:use :cl :fiveam :arithmetic)
  (:export :run-tests))

;; Enter the testing package
(in-package :arithmetic-test)

;; Define and enter a new FiveAM test-suite
(def-suite arithmetic-suite)
(in-suite arithmetic-suite)

(test dough-ratio "Calculate the grams of dough needed for given number and size of pizzas"
  (is (= (dough-calculator 4 30) 1648))
  (is (= (dough-calculator 2 35) 895))
  (is (= (dough-calculator 6 20) 2048))
  (is (= (dough-calculator 1 15) 306))
  (is (= (dough-calculator 5 10) 1353)))

(test splash-of-sauces "Calculates the diameter of a pizza from the amount of sauce applied"
  (is (= (size-from-sauce 250) 16.286750396764d0))
  (is (= (size-from-sauce 100) 10.300645387285057d0))
  (is (= (size-from-sauce 330) 18.712051592547777d0))
  (is (= (size-from-sauce 510) 23.26213245840639d0))
  (is (= (size-from-sauce 680) 26.86079687357132d0)))

;; Either provides human-readable results to the user or machine-readable
;; results to the test runner. The default upon calling `(run-tests)` is to
;; explain the results in a human-readable way
(defun run-tests (&optional (explain t))
  (let ((tests (run 'arithmetic-suite))) ; Run the tests once
    (if explain (explain! tests) tests))) ; Optionally explain the results
