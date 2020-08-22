(defpackage conditionals
  (:use :cl)
  (:export :pal-picker :habitat-fitter))

(in-package :conditionals)

(defun pal-picker (personality)
  (case personality
    (:lazy "Cat")
    (:energetic "Dog")
    (:quiet "Fish")
    (:hungry "Rabbit")
    (:talkative "Bird")
    (otherwise "I don't know... A dragon?")))

(defun habitat-fitter (weight)
  (cond
    ((>= weight 40) :massive)
    ((>= weight 20) :large)
    ((>= weight 10) :medium)
    ((> weight 0) :small)
    (t :just-your-imagination)))
