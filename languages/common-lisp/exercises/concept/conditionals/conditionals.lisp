(defpackage conditionals
  (:use :cl)
  (:export :pal-picker))

(in-package :conditionals)

(defun pal-picker (personality)
  (case personality
    (:lazy "Cat")
    (:energetic "Dog")
    (:quiet "Fish")
    (:hungry "Rabbit")
    (:talkative "Bird")
    (otherwise "I don't know... A dragon?")))
