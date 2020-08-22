(defpackage conditionals
  (:use :cl)
  (:export :pal-picker :habitat-fitter :feeding-time-p
           :pet :play-fetch))

(in-package :conditionals)

(defun pal-picker (personality))

(defun habitat-fitter (weight))

(defun feeding-time-p (fullness))

(defun pet (pet))

(defun play-fetch (pet))
