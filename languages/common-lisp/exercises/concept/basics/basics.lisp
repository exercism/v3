(defpackage basics
  (:use :cl)
  (:export :lennys-favorite-food :lennys-secret-keyword
           :is-an-atom-p :is-a-cons-p :first-thing :rest-of-it))

(in-package basics)

;; Evaluates to the symbol LASAGNA
(defun lennys-favorite-food ())

;; Evaluates to the keyword :ALIENS-ARE-REAL
(defun lennys-secret-keyword ())

;; Evaluates to T if THING is an atom, NIL otherwise
(defun is-an-atom-p (thing))

;; Evaluates to T if THING is a cons, NIL otherwise
(defun is-a-cons-p (thing))

;; Evaluates to the first part of CONS
(defun first-thing (cons))

;; Evaluates to the 'rest' of the CONS
(defun rest-of-it (cons))
