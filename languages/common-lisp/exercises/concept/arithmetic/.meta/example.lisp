(defpackage arithmetic
  (:use :cl)
  (:export :dough-calculator :pizzas-per-cube
           :size-from-sauce :fair-share-p))

(in-package :arithmetic)

(defun dough-calculator (pizzas diameter)
  (round (* pizzas (+ (* pi diameter (/ 45 20)) 200))))

(defun size-from-sauce (sauce)
  (sqrt (/ (* sauce 10) (* 3 pi))))

(defun pizzas-per-cube (cube-size diameter)
  (floor (/ (* 0.5 (expt cube-size 3)) (* 3 pi (expt (/ diameter 2) 2)))))

(defun fair-share-p (pizzas friends)
  (= 0 (mod (* pizzas 8) friends)))
