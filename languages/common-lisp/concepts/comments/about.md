Single-line comments in Common Lisp follow a couple of conventions:

```lisp
;;;; A short title for a block of code
;;; A longer description of a block of code
;; Describe the code below this line
; Describe the code before the semi-colon (;) on this line
```

Additionally, multi-line comments are possible using `#|` and `|#`:

```lisp
#|
This entire block of text is a comment
and can span several lines!

How cool is that?!
|#
```