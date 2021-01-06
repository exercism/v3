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

(test preparation-time
  (is (= 0 (preparation-time)))
  (is (= 19 (preparation-time 'sauce)))
  (is (= 95 (preparation-time 'sauce 'cheese 'left-handed-macaroni 'cheese 'cheese))))

(test remaining-minutes-in-oven-normal
  (is (= 337 (remaining-minutes-in-oven)))
  (is (= 337 (remaining-minutes-in-oven :normal))))

(test remaining-mintues-in-oven-shorter
  (is (= 237 (remaining-minutes-in-oven :shorter)))
  (is (= 137 (remaining-minutes-in-oven :very-short))))

(test remaining-minutes-in-oven-longer
  (is (= 437 (remaining-minutes-in-oven :longer)))
  (is (= 537 (remaining-minutes-in-oven :very-long))))

(test remaining-minutes-in-oven-very-very-short
  (is (= 0   (remaining-minutes-in-oven nil))))

(test splitting-leftovers
  (is (= 5 (split-leftovers :weight 20 :human 10 :alien 5)))
  (is (= 4 (split-leftovers :alien 12 :weight 20 :human 4))))

(test splitting-leftovers-defaulting-containers
  (is (= 5 (split-leftovers :weight 20 :human 5)))
  (is (= 5 (split-leftovers :weight 20 :alien 5)))
  (is (= 0 (split-leftovers :weight 20))))

(test splitting-leftovers-default-weight
  (is (eq :just-split-it (split-leftovers :human 5 :alien 5)))
  (is (eq :looks-like-someone-was-hungry (split-leftovers :weight NIL))))

;; Either provides human-readable results to the user or machine-readable
;; results to the test runner. The default upon calling `(run-tests)` is to
;; explain the results in a human-readable way
(defun run-tests (&optional (explain t))
  (let ((tests (run 'lillys-lasagna-leftovers-suite))) ; Run the tests once
    (if explain (explain! tests) tests))) ; Optionally explain the results
