(defpackage sameness
  (:use :cl)
  (:export :robot))

(in-package :sameness)

(defun robot (room-id)
  "Return a key to use for the doors in the room designated by ROOM-ID."
  (case room-id
    (:room-object-identity #'eq)
    (:room-characters #'eql)
    (:room-numbers #'eql)
    (:room-cons-of-symbols #'equal)
    (:room-cons-of-chars #'equal)
    (:room-cons-of-numbers #'equal)
    (:room-arrays #'equal)
    (:room-strings #'equal)
    (:room-case-insensitive-chars #'equalp)
    (:room-number-of-different-types #'equalp)
    (:room-case-insensitive-strings #'equalp)
    (:room-cons-case-insensitive-chars #'equalp)
    (:room-cons-number-of-different-types #'equalp)
    (:room-arrays-looser-equal #'equalp)
    (:room-structures #'equalp)
    (:room-hash-table #'equalp)
    (t (constantly nil))))
