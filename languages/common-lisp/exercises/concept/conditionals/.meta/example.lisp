(defpackage conditionals
  (:use :cl)
  (:export :pal-picker :habitat-fitter :feeding-time-p
           :pet :play-fetch))

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

(defun feeding-time-p (fullness)
  (if (> fullness 20)
      "All is well."
      "It's feeding time!"))

(defun pet (pet)
  (when (string= pet "Fish")
    "Maybe not with this pet..."))

(defun play-fetch (pet)
  (unless (string= pet "Dog")
    "Maybe not with this pet..."))
