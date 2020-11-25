(defpackage :lillys-lasagna
  (:use :cl)
  (:export :expected-time-in-oven
           :remaining-minutes-in-oven
           :preparation-time-in-minutes
           :elapsed-time-in-minutes))

(in-package :lillys-lasagna)

(defun expected-time-in-oven ()
  "Number of minutes Lasagna should be in oven."
  337)

(defun remaining-minutes-in-oven (in-oven)
  "Number of minutes remaining when Lasagna has been in the oven for IN-OVEN minutes."
  (- (expected-time-in-oven) in-oven))

(defun preparation-time-in-minutes (num-layers)
  "Number of minutes for preparation of Lasagna with NUM-LAYERS number of layers."
  (* num-layers 19))

(defun elapsed-time-in-minutes (num-layers in-oven)
  "Number of elapsed minutes given NUM-LAYERS on the Lasagna and IN-OVEN minutes
it has already been in the oven."
  (+ (preparation-time-in-minutes num-layers) in-oven))
