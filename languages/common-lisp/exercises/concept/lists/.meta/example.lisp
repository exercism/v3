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

(defun new-list ()
  (list))

(defun list-of-things (thing1 thing2 thing3)
  (list thing1 thing2 thing3))

(defun add-to-list (item list)
  (cons item list))

(defun first-thing (list)
  (first list))

(defun second-thing (list)
  (second list))

(defun third-thing (list)
  (third list))

(defun twenty-third-thing (list)
  (nth 23 list))

(defun remove-first-item (list)
  (rest list))

(defun list-append (list1 list2)
  (append list1 list2))

(defun just-how-long (list)
  (length list))
