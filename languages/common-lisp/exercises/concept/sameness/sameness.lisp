(defpackage sameness
  (:use :cl)
  (:export
   :key-object-indentity
   :key-numbers
   :key-looser-numbers
   :key-chars
   :key-insensitive-chars
   :key-strings
   :key-insensitive-string
   :key-cons-symbols
   :key-cons-chars
   :key-cons-numbers
   :key-cons-insensitive-chars
   :key-cons-looser-numbers
   :key-arrays
   :key-arrays-looser-equal))

(in-package :sameness)

(defun key-object-indentity (x y))
(defun key-numbers (x y))
(defun key-looser-numbers (x y))
(defun key-chars (x y))
(defun key-insensitive-chars (x y))
(defun key-strings (x y))
(defun key-insensitive-string (x y))
(defun key-cons-symbols (x y))
(defun key-cons-chars (x y))
(defun key-cons-numbers (x y))
(defun key-cons-insensitive-chars (x y))
(defun key-cons-looser-numbers (x y))
(defun key-arrays (x y))
(defun key-arrays-looser-equal (x y))
