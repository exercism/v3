(defpackage sameness
  (:use :cl)
  (:export :robot))

(in-package :sameness)

(defun robot (room-id)
  "Return a key to use for the doors in the room designated by ROOM-ID."
  (case room-id
    ;; add forms such as:
    ;; (room-id-13 #'equalp)

    (t (constantly t))))
