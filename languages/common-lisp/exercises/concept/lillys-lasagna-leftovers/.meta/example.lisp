(defpackage :lillys-lasagna-leftovers
  (:use :cl)
  (:export
   :preparation-time
   :remaining-minutes-in-oven
   :split-leftovers))

(in-package :lillys-lasagna-leftovers)

(defun preparation-time (&rest layers) (* 19 (length layers)))

(defun remaining-minutes-in-oven (&optional (length :normal))
  (+ 337 (if (not length) -337
             (case length
               (:longer 100)
               (:very-long 200)
               (:shorter -100)
               (:very-short -200)
               (t 0)))))

(defun split-leftovers (&key (weight nil weight-supplied-p) (human 10) (alien 10))
  (cond ((and weight-supplied-p (not weight)) :looks-like-someone-was-hungry)
        ((not weight) :just-split-it)
        (t (- weight human alien))))
