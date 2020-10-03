(defpackage lists
  (:use :cl)
  (:export :new-list
           :list-of-things
           :add-to-list
           :first-thing
           :second-thing
           :third-thing
           :twenty-third-thing
           :remove-first-item
           :on-the-list-p
           :list-append
           :just-how-long
           :part-of-list
           :list-reverse))

(in-package :lists)

(defun new-list () 'this-is-wrong)

(defun list-of-things (thing1 thing2 thing3))

(defun add-to-list (item list))

(defun first-thing (list))

(defun second-thing (list))

(defun third-thing (list))

(defun twenty-third-thing (list))

(defun remove-first-item (list))

(defun on-the-list-p (item list))

(defun list-append (list1 list2))

(defun just-how-long (list))

(defun part-of-list (list start num))

(defun list-reverse (list))
