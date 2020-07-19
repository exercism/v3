;; Ensures that sameness.lisp and the testing library are always loaded
(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "sameness")
  (ql:quickload :fiveam))

;; Defines the testing package with symbols from sameness and FiveAM in scope
;; The `run-tests` function is exported for use by both the user and test-runner
(defpackage sameness-test
  (:use :cl :fiveam :sameness)
  (:export :run-tests))

;; Enter the testing package
(in-package :sameness-test)

;; forward declaration of test utilities
(declaim (ftype (function) open-room))
(defparameter +rooms+ nil)

;; Define and enter a new FiveAM test-suite
(def-suite sameness-suite)
(in-suite sameness-suite)

(test the-maze-of-object-identity
  (is (eq 'victory (open-room :room-object-identity #'key-object-indentity))))

(test the-maze-of-numbers
  (is (eq 'victory (open-room :room-numbers #'key-numbers)))
  (is (eq 'victory (open-room :room-number-of-different-types #'key-looser-numbers))))

(test the-maze-of-characters
  (is (eq 'victory (open-room :room-characters #'key-chars)))
  (is (eq 'victory (open-room :room-case-insensitive-chars #'key-insensitive-chars))))

(test the-maze-of-strings
  (is (eq 'victory (open-room :room-strings #'key-strings)))
  (is (eq 'victory (open-room :room-case-insensitive-strings #'key-insensitive-string))))

(test the-maze-of-conses
  (is (eq 'victory (open-room :room-cons-of-symbols #'key-cons-symbols)))
  (is (eq 'victory (open-room :room-cons-of-chars #'key-cons-chars)))
  (is (eq 'victory (open-room :room-cons-of-numbers #'key-cons-numbers)))
  (is (eq 'victory (open-room :room-cons-case-insensitive-chars #'key-cons-insensitive-chars)))
  (is (eq 'victory (open-room :room-cons-number-of-different-types #'key-cons-looser-numbers))))

(test the-maze-of-arrays
  (is (eq 'victory (open-room :room-arrays #'key-arrays)))
  (is (eq 'victory (open-room :room-arrays-looser-equal #'key-arrays-looser-equal))))

;; Either provides human-readable results to the user or machine-readable
;; results to the test runner. The default upon calling `(run-tests)` is to
;; explain the results in a human-readable way
(defun run-tests (&optional (explain t))
  (let ((tests (run 'sameness-suite))) ; Run the tests once
    (if explain (explain! tests) tests))) ; Optionally explain the results

;;;
;;; ==================================================
;;; Test Implementation Details
;;;
(defun open-room (room-id key-fn)
  (let ((room (cdr (assoc room-id +rooms+))))
    (loop for (door . behind-the-door) in room
       when (apply key-fn door) do (return behind-the-door)
         finally (return 'room-explodes))))

(defparameter +an-array+ #(1 2 3))
(defparameter +a-similar-but-different-array+ #(1 2 3))
(defparameter +a-different-array+ #(1 2 4))

(defparameter +rooms+
  `(
    (:room-object-identity
     . ((("wrong" "WRONG") . explosion)
        ((2 2.0) . explosion)
        ((lisp LISP) . victory)))

    (:room-characters
     . (((#\a #\A) . explosion)
        ((#\a #\a) . victory)))

    (:room-numbers
     . (((1.0 1) . explosion)
        ((1.0 1.0) . victory)))

    (:room-cons-of-symbols
     . ((((a . b) (a . c)) . explosion)
        (((a . b) (a . b)) . victory)))

    (:room-cons-of-chars
     . ((((#\a . #\b) (#\A . #\b)) . explosion)
        (((#\a . #\b) (#\a . #\b)) . victory)))

    (:room-cons-of-numbers
     . ((((1 . 2) (1 . 2.0)) . explosion)
        (((1 . 2) (1 . 2)) . victory)))

    (:room-arrays
     . (((,+an-array+ ,+a-similar-but-different-array+) . explosion)
        ((,+an-array+ ,+an-array+) . victory)))

    (:room-strings
     . ((("wrong" "WRONG") . explosion)
        (("lisp" "lisp") . victory)))

    (:room-case-insensitive-chars
     . (((#\a #\b) . explosion)
        ((#\a #\A) . victory)))

    (:room-number-of-different-types
     . (((1.0 1.1) . explosion)
        ((1 1.0) . victory)))

    (:room-case-insensitive-strings
     . ((("right" "wrong") . explosion)
        (("lisp" "LISP") . victory)))

    (:room-cons-case-insensitive-chars
     . ((((1 . 1) (1 . 2)) . explosion)
        (((1 . 1) (1.0 . 1.0)) . victory)))

    (:room-cons-number-of-different-types
     . ((((#\a . #\b) (#\a . #\c)) . explosion)
        (((#\a . #\b) (#\A . #\B)) . victory)))

    (:room-arrays-looser-equal
     . (((,+an-array+ ,+a-different-array+) . explosion)
        ((,+an-array+ ,+a-similar-but-different-array+) . victory))))
  "Rooms are a sequence of pairs of a DOOR and a RESULT. A DOOR is a sequence of
things which will be given to the key. If a KEY opens a DOOR, the room will be
opened and evaluate to RESULT")
