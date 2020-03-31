(defpackage #:basics
  (:use :cl)
  (:export :gimme-bar-symbol :gimme-baz-keyword :speak-the-truth :lie-to-me
           :is-an-atom-p :is-a-cons-p :first-thing :rest-of-it))

(in-package :basics)

;; Returns the symbol BAR
(defun gimme-bar-symbol ())

;; Returns the keyword BAZ
(defun gimme-baz-keyword ())

;; Return something true
(defun speak-the-truth ())

;; Return something false
(defun lie-to-me ())

;; Evaluates to T if THING is an atom, NIL otherwise
(defun is-an-atom-p (thing))

;; Evaluates to T if THING is a cons, NIL otherwise
(defun is-a-cons-p (thing))

;; Returns the first part of CONS
(defun first-thing (cons))

;; Returns the 'rest' of the CONS
(defun rest-of-it (cons))
