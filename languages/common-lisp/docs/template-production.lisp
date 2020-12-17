(in-package #:cl-user)
(defpackage #:exercise
  (:use #:common-lisp)
  (:shadow #:list)
  (:export #:function-under-test))

(in-package #:exercise)

(defun function-under-test (args))
