(load "basics.lisp")
(ql:quickload :fiveam)
(defpackage basics-test
  (:use :cl :fiveam :basics)
  (:export :run-tests))
(in-package :basics-test)

(def-suite basics-suite)
(in-suite basics-suite)

(test symbols
  (is-true (symbolp (basics:lennys-favorite-food)))
  (is-false (keywordp (basics:lennys-favorite-food)))

  (is-true (keywordp (basics:lennys-secret-keyword))))

(test atoms
  (is-true (basics:is-an-atom-p 5))
  (is-true (basics:is-an-atom-p 5.5))
  (is-true (basics:is-an-atom-p 'a))
  (is-true (basics:is-an-atom-p "hello"))
  (is-true (basics:is-an-atom-p t))
  (is-true (basics:is-an-atom-p :a-keyword))
  (is-true (basics:is-an-atom-p #'atom))
  (is-true (basics:is-an-atom-p #'consp))
  (is-true (basics:is-an-atom-p nil))

  (is-false (basics:is-an-atom-p '(1 2 3)))
  (is-false (basics:is-an-atom-p '(a . b)))
  (is-false (basics:is-an-atom-p (list 1 2 3)))
  (is-false (basics:is-an-atom-p (cons 'a 'b))))

(test conses
  (is-true (basics:is-a-cons-p '(a . b)))
  (is-true (basics:is-a-cons-p (cons 'a 'b)))
  (is-true (basics:is-a-cons-p (list 1 2 3)))

  (is-false (basics:is-a-cons-p 5))
  (is-false (basics:is-a-cons-p 5.5))
  (is-false (basics:is-a-cons-p 'a))
  (is-false (basics:is-a-cons-p "hello"))
  (is-false (basics:is-a-cons-p t))
  (is-false (basics:is-a-cons-p :a-keyword))
  (is-false (basics:is-a-cons-p #'atom))
  (is-false (basics:is-a-cons-p #'consp))
  (is-false (basics:is-a-cons-p nil)))

(test car-and-cdr
  (is (equal (basics:first-thing (cons 'a 'b)) 'a))
  (is (equal (basics:first-thing (list 'a 'b)) 'a))

  (is (equal (basics:rest-of-it (cons 'a 'b)) 'b))
  (is (equal (basics:rest-of-it (cons 'a (cons 'b nil))) '(b)))
  (is (equal (basics:rest-of-it (list 'a 'b)) '(b)))
  (is (equal (basics:rest-of-it (cons 'a nil)) nil))
  (is (equal (basics:rest-of-it (list 'a)) nil)))

(defun run-tests ()
  (run! 'basics-suite))
