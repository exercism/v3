(load "sexpr.lisp")
(ql:quickload :fiveam)
(defpackage #:sexpr-test
  (:use :cl :fiveam :sexpr)
  (:export :run-test-suite :sexpr-suite))
(in-package :sexpr-test)

(load "./sexpr")

(def-suite sexpr-suite)
(in-suite sexpr-suite)

(test atoms
      (is-true (sexpr:is-an-atom-p 5))
      (is-true (sexpr:is-an-atom-p 5.5))
      (is-true (sexpr:is-an-atom-p 'a))
      (is-true (sexpr:is-an-atom-p "hello"))
      (is-true (sexpr:is-an-atom-p t))
      (is-true (sexpr:is-an-atom-p :a-keyword))
      (is-true (sexpr:is-an-atom-p #'atom))
      (is-true (sexpr:is-an-atom-p #'consp))
      (is-true (sexpr:is-an-atom-p nil))

      (is-false (sexpr:is-an-atom-p '(1 2 3)))
      (is-false (sexpr:is-an-atom-p '(a . b)))
      (is-false (sexpr:is-an-atom-p (list 1 2 3)))
      (is-false (sexpr:is-an-atom-p (cons 'a 'b))))

(test conses
      (is-true (sexpr:is-a-cons-p '(a . b)))
      (is-true (sexpr:is-a-cons-p (cons 'a 'b)))
      (is-true (sexpr:is-a-cons-p (list 1 2 3)))

      (is-false (sexpr:is-a-cons-p 5))
      (is-false (sexpr:is-a-cons-p 5.5))
      (is-false (sexpr:is-a-cons-p 'a))
      (is-false (sexpr:is-a-cons-p "hello"))
      (is-false (sexpr:is-a-cons-p t))
      (is-false (sexpr:is-a-cons-p :a-keyword))
      (is-false (sexpr:is-a-cons-p #'atom))
      (is-false (sexpr:is-a-cons-p #'consp))
      (is-false (sexpr:is-a-cons-p nil)))

(test car-and-cdr
      (is (equal (sexpr:first-thing (cons 'a 'b)) 'a))
      (is (equal (sexpr:first-thing (list 'a 'b)) 'a))

      (is (equal (sexpr:rest-of-it (cons 'a 'b)) 'b))
      (is (equal (sexpr:rest-of-it (cons 'a (cons 'b nil))) '(b)))
      (is (equal (sexpr:rest-of-it (list 'a 'b)) '(b)))
      (is (equal (sexpr:rest-of-it (cons 'a nil)) nil))
      (is (equal (sexpr:rest-of-it (list 'a)) nil)))

(defun run-tests ()
  (run! 'sexpr-suite))
