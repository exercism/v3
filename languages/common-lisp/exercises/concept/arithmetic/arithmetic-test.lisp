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

(test cheese-please "Calculates the number of pizzas of a certain size that can be made from an amount of cheese"
  (is (= (pizzas-per-cube 25 30) 3))
  (is (= (pizzas-per-cube 15 20) 1))
  (is (= (pizzas-per-cube 100 40) 132))
  (is (= (pizzas-per-cube 5 10) 0))
  (is (= (pizzas-per-cube 45 15) 85)))

(test fair-share "Calculates if some number of pizzas can be evenly divided between friends"
  (is-true  (fair-share-p 3 4))
  (is-false (fair-share-p 2 3))
  (is-false (fair-share-p 4 5))
  (is-true  (fair-share-p 4 8))
  (is-true  (fair-share-p 1 4))
  (is-true  (fair-share-p 21 7))
  (is-false (fair-share-p 11 10))
  (is-true  (fair-share-p 0 5))
  (is-false (fair-share-p 17 5))
  (is-true  (fair-share-p 16 64)))

;; Either provides human-readable results to the user or machine-readable
;; results to the test runner. The default upon calling `(run-tests)` is to
;; explain the results in a human-readable way
(defun run-tests (&optional (explain t))
  (let ((tests (run 'arithmetic-suite))) ; Run the tests once
    (if explain (explain! tests) tests))) ; Optionally explain the results
