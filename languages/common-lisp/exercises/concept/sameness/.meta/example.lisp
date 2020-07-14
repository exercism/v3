(defpackage sameness
  (:use :cl)
  (:export :robot))

(in-package :sameness)

(defun robot (room-id)
  "Return a key to use for the doors in the room designated by ROOM-ID."
  (case room-id
    (:room-object-identity #'eq)
    (:room-numbers #'eql)
    (:room-number-of-different-types #'equalp)
    (:room-characters #'eql)
    (:room-case-insensitive-chars #'equalp)
    (:room-strings #'equal)
    (:room-case-insensitive-strings #'equalp)
    (:room-cons-of-symbols #'equal)
    (:room-cons-of-chars #'equal)
    (:room-cons-of-numbers #'equal)
    (:room-cons-case-insensitive-chars #'equalp)
    (:room-cons-number-of-different-types #'equalp)
    (:room-arrays #'equal)
    (:room-arrays-looser-equal #'equalp)
    (t (constantly nil))))
