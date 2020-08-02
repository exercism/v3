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
  (is (eq 'victory (open-room :room-object-identity
                              #'key-object-identity))))

(test the-maze-of-numbers
  (is (eq 'victory (open-room :room-numbers
                              #'key-numbers)))
  (is (eq 'victory (open-room :room-numbers-of-different-types
                              #'key-numbers-of-different-types))))

(test the-maze-of-characters
  (is (eq 'victory (open-room :room-characters #'key-characters)))
  (is (eq 'victory (open-room :room-characters-case-insensitively
                              #'key-characters-case-insensitively))))

(test the-maze-of-strings
  (is (eq 'victory (open-room :room-strings #'key-strings)))
  (is (eq 'victory (open-room :room-strings-case-insensitively
                              #'key-strings-case-insensitively))))

(test the-maze-of-conses
  (is (eq 'victory (open-room :room-conses-of-symbols
                              #'key-conses-of-symbols)))
  (is (eq 'victory (open-room :room-conses-of-characters
                              #'key-conses-of-characters)))
  (is (eq 'victory (open-room :room-conses-of-numbers
                              #'key-conses-of-numbers)))
  (is (eq 'victory (open-room :room-conses-of-characters-case-insensitively
                              #'key-conses-of-characters-case-insensitively)))
  (is (eq 'victory (open-room :room-conses-of-numbers-of-different-types
                              #'key-conses-of-numbers-of-different-types))))

(test the-maze-of-arrays
  (is (eq 'victory (open-room :room-arrays
                              #'key-arrays)))
  (is (eq 'victory (open-room :room-arrays-loosely
                              #'key-arrays-loosely))))

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

    (:room-conses-of-symbols
     . ((((a . b) (a . c)) . explosion)
        (((a . b) (a . b)) . victory)))

    (:room-conses-of-characters
     . ((((#\a . #\b) (#\A . #\b)) . explosion)
        (((#\a . #\b) (#\a . #\b)) . victory)))

    (:room-conses-of-numbers
     . ((((1 . 2) (1 . 2.0)) . explosion)
        (((1 . 2) (1 . 2)) . victory)))

    (:room-arrays
     . (((,+an-array+ ,+a-similar-but-different-array+) . explosion)
        ((,+an-array+ ,+an-array+) . victory)))

    (:room-strings
     . ((("wrong" "WRONG") . explosion)
        (("lisp" "lisp") . victory)))

    (:room-characters-case-insensitively
     . (((#\a #\b) . explosion)
        ((#\a #\A) . victory)))

    (:room-numbers-of-different-types
     . (((1.0 1.1) . explosion)
        ((1 1.0) . victory)))

    (:room-strings-case-insensitively
     . ((("right" "wrong") . explosion)
        (("lisp" "LISP") . victory)))

    (:room-conses-of-characters-case-insensitively
     . ((((#\a . #\b) (#\a . #\c)) . explosion)
        (((#\a . #\b) (#\A . #\B)) . victory)))

    (:room-conses-of-numbers-of-different-types
     . ((((1 . 1) (1 . 2)) . explosion)
        (((1 . 1) (1.0 . 1.0)) . victory)))

    (:room-arrays-loosely
     . (((,+an-array+ ,+a-different-array+) . explosion)
        ((,+an-array+ ,+a-similar-but-different-array+) . victory))))
  "Rooms are a sequence of pairs of a DOOR and a RESULT. A DOOR is a sequence of
things which will be given to the key. If a KEY opens a DOOR, the room will be
opened and evaluate to RESULT")
