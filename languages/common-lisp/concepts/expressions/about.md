## Summary

This exercise's goal was to begin learning about Common Lisp with a small
example of its most basic data structures (atoms and conses) and to get to grips
with symbols in Common Lisp

Along the way you saw examples of how to write functions as well as learned how
to load code and run it in your Common Lisp implementation.

Future exercises will dive much deeper into function definition and how quoting
can be used to manipulate code was if it were data.

## Diving Deeper

### Comments

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

### Quoting

In the introduction, quoting `FOO` makes it act a lot like a keyword, but unlike
keywords (which can only be symbols), all S-Expressions can be quoted:

```lisp
;; This line is evaluated as code
(gimme-foo)  ; => FOO

;; This line is treated as data
'(gimme-foo) ; => (GIMME-FOO)
```

In case you didn't see the hints, here is an excellent [Stack Overflow
Answer][so-quoting] discussing quoting in a bit more detail.

## Reference

```lisp
;; Returning a symbol from a `defun`
(defun gimme-foo () 'foo)
(gimme-foo) ; => FOO

;; The same, but as a keyword
(defun gimme-bar () :bar)
(gimme-bar) ; => :BAR

;; Testing for atoms
(atom 'foo)       ; => T
(atom '(bar baz)) ; => NIL

;; Testing for cons
(consp 'foo)       ; => NIL
(consp '(bar baz)) ; => T

;; First item of a cons
(car '(one two three))   ; => ONE
(first '(one two three)) ; => ONE

;; Rest of a cons
(cdr '(one two three))  ; => (TWO THREE)
(rest '(one two three)) ; => (TWO THREE)
```

[so-quoting]: https://stackoverflow.com/questions/134887/when-to-use-or-quote-in-lisp
