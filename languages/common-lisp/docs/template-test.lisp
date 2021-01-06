(ql:quickload "lisp-unit")
#-xlisp-test (load "exercise")

(defpackage #:exercise-test
  (:use #:common-lisp #:lisp-unit))

(in-package #:exercise-test)

;;;
;;; tests go here...
;;; /e.g./ (define-test a-test (assert-equal 1 2))
;;;

#-xlisp-test
(let ((*print-errors* t)
      (*print-failures* t))
  (run-tests :all :exercise))
