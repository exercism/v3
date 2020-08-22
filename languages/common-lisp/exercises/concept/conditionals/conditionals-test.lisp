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

(test pick-a-pal "Maps personality traits to fitting pets"
  (is (string= (pal-picker :lazy) "Cat"))
  (is (string= (pal-picker :energetic) "Dog"))
  (is (string= (pal-picker :quiet) "Fish"))
  (is (string= (pal-picker :hungry) "Rabbit"))
  (is (string= (pal-picker :talkative) "Bird"))
  (is (string= (pal-picker :fireproof) "I don't know... A dragon?")))

(test natural-habitat "Maps pet weights to habitat sizes"
  (is (eql (habitat-fitter 100) :massive))
  (is (eql (habitat-fitter 40) :massive))
  (is (eql (habitat-fitter 39) :large))
  (is (eql (habitat-fitter 20) :large))
  (is (eql (habitat-fitter 19) :medium))
  (is (eql (habitat-fitter 10) :medium))
  (is (eql (habitat-fitter 9) :small))
  (is (eql (habitat-fitter 1) :small))
  (is (eql (habitat-fitter 0) :just-your-imagination))
  (is (eql (habitat-fitter -5) :just-your-imagination)))

(test we-feast "Determines whether the food-bowl needs refilling from its fullness"
  (is (string= (feeding-time-p 10) "It's feeding time!"))
  (is (string= (feeding-time-p 36) "All is well."))
  (is (string= (feeding-time-p 74) "All is well."))
  (is (string= (feeding-time-p 3) "It's feeding time!"))
  (is (string= (feeding-time-p 90) "All is well.")))

;; Either provides human-readable results to the user or machine-readable
;; results to the test runner. The default upon calling `(run-tests)` is to
;; explain the results in a human-readable way
(defun run-tests (&optional (explain t))
  (let ((tests (run 'conditionals-suite))) ; Run the tests once
    (if explain (explain! tests) tests))) ; Optionally explain the results
