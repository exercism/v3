;; Ensures that sameness.lisp and the testing library are always loaded
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; (load "sameness")
  (load "./.meta/example.lisp")
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

(defparameter +mazes+
  '((:maze-1 . ((("wrong" "WRONG") . explosion)
                ((2 2.0) . explosion)
                ((lisp LISP) . victory)))
    (:maze-2 . (((#\a #\A) . explosion)
                ((#\a #\a) . victory)))
    (:maze-3 . (((1.0 1) . explosion)
                ((1.0 1.0) . victory))))
  "Mazes are a sequence of pairs of a DOOR and a RESULT. A DOOR is a sequence of
things which will be given to the key. If a KEY opens a DOOR, the maze will
evaluate to RESULT")

(test the-maze-of-object-equality
  (is (eq 'victory (run-maze :maze-1 #'sameness:robot))))

;; tests that must use eql (numbers of same type, characters)
(test the-maze-of-characters
  (is (eq 'victory (run-maze :maze-2 #'sameness:robot))))
(test the-maze-of-numbers
  (is (eq 'victory (run-maze :maze-3 #'sameness:robot))))

;; tests that must use equal (lists, arrays (eq), strings)

;; tests that must use equalp (characters (case insensitive) arrays (numbers for
;; example), strings (case insensitive))


(defun run-maze (maze-id robot)
  "Utility function to run a particular maze (by MAZE-ID) with a particular ROBOT."
  (let ((key (funcall robot maze-id))
        (maze (cdr (assoc maze-id +mazes+))))
    (loop for (door . behind-the-door) in maze
          when (apply key door) do (return behind-the-door)
            finally (return 'sad-trombone))))

;; Either provides human-readable results to the user or machine-readable
;; results to the test runner. The default upon calling `(run-tests)` is to
;; explain the results in a human-readable way
(defun run-tests (&optional (explain t))
  (let ((tests (run 'sameness-suite))) ; Run the tests once
    (if explain (explain! tests) tests))) ; Optionally explain the results
