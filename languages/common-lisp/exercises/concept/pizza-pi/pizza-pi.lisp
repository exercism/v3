(defpackage :pizza-pi
  (:use :cl)
  (:export :dough-calculator :pizzas-per-cube
           :size-from-sauce :fair-share-p))

(in-package :pizza-pi)

(defun dough-calculator (pizzas diameter))

(defun size-from-sauce (sauce))

(defun pizzas-per-cube (cube-size diameter))

(defun fair-share-p (pizzas friends))
