#lang racket

(provide hello)

(define (hello [name "World"])
  (string-append "Hello, " name "!"))
