(load "basics.lisp")
(ql:quickload :fiveam)
(defpackage basics-test
  (:use :cl :fiveam :basics)
  (:export :run-tests))
(in-package :basics-test)

(def-suite basics-suite)
(in-suite basics-suite)

(test symbols
  (is-true (symbolp (lennys-favorite-food)))
  (is-false (keywordp (lennys-favorite-food)))

  (is-true (keywordp (lennys-secret-keyword))))

(test atoms
  (is-true (is-an-atom-p 5))
  (is-true (is-an-atom-p 5.5))
  (is-true (is-an-atom-p 'a))
  (is-true (is-an-atom-p "hello"))
  (is-true (is-an-atom-p t))
  (is-true (is-an-atom-p :a-keyword))
  (is-true (is-an-atom-p #'atom))
  (is-true (is-an-atom-p #'consp))
  (is-true (is-an-atom-p nil))

  (is-false (is-an-atom-p '(1 2 3)))
  (is-false (is-an-atom-p '(a . b)))
  (is-false (is-an-atom-p (list 1 2 3)))
  (is-false (is-an-atom-p (cons 'a 'b))))

(test conses
  (is-true (is-a-cons-p '(a . b)))
  (is-true (is-a-cons-p (cons 'a 'b)))
  (is-true (is-a-cons-p (list 1 2 3)))

  (is-false (is-a-cons-p 5))
  (is-false (is-a-cons-p 5.5))
  (is-false (is-a-cons-p 'a))
  (is-false (is-a-cons-p "hello"))
  (is-false (is-a-cons-p t))
  (is-false (is-a-cons-p :a-keyword))
  (is-false (is-a-cons-p #'atom))
  (is-false (is-a-cons-p #'consp))
  (is-false (is-a-cons-p nil)))

(test car-and-cdr
  (is (equal (first-thing (cons 'a 'b)) 'a))
  (is (equal (first-thing (list 'a 'b)) 'a))

  (is (equal (rest-of-it (cons 'a 'b)) 'b))
  (is (equal (rest-of-it (cons 'a (cons 'b nil))) '(b)))
  (is (equal (rest-of-it (list 'a 'b)) '(b)))
  (is (equal (rest-of-it (cons 'a nil)) nil))
  (is (equal (rest-of-it (list 'a)) nil)))

(defun run-tests ()
  (run! 'basics-suite))
