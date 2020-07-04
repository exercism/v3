(defpackage sameness
  (:use :cl)
  (:export :robot))

(in-package :sameness)

(defun robot (maze-id)
  "Return a key to use for the doors in the maze designated by MAZE-ID."
  (case maze-id
    ;; add forms such as:
    ;; (maze-id-13 #'equalp)

    (t (constantly t))))
