(defpackage #:sexpr
  (:use :cl)
  (:export :is-an-atom-p :is-a-cons-p))

(defpackage #:sexpr-test
  (:use :cl :fiveam :sexpr)
  (:export :sexpr-suite))
