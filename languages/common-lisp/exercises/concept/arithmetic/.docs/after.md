## Summary

The goal of this exercise was to get comfortable with prefix-arithmetic in
Common Lisp and learn about integer and floating-point numbers.

In addition to the traditional addition, subtraction, multiplication, and
division operations, you encountered different rounding functions as well as
functions that take the square-root or raise some number to a given power.

With these tools, you should be well equipped to handle arithmetic problems in
Common Lisp.

## Diving Deeper

### Arithmetic Operators With a Single Argument
As a small quirk, the `-` and `/` operators have a special meanings when applied
to only one number:

```lisp
;; A single number passed to `-` is simply negated
(- 4)   ; => -4
(- -32) ; => 32
;; A single number passed to `/` returns the reciprocal
(/ 8)   ; => 1/8
(/ 0.1) ; => 10.0
```

### Arbitrary Precision Integers

In Common Lisp, integers can be arbitrarily large or small, so there is no need
to worry about "overflowing" when working with them. While some other languages
might experience a loss of precision or wrap large numbers, the size of integers
in Common Lisp is limited only by the amount of memory available.

```lisp
;; Big integers are no problem
(expt 2 200) ; => 1606938044258990275541962092341162602522202993782792835301376
```

### More Numerical Types & Operations

While integers and floats are the most common numerical types in Lisp, there
also exists support for rational and complex numbers. These will likely be
touched on in a future exercise, but to hold you over, you can take a peek at
[The Common Lisp
Cookbook](https://lispcookbook.github.io/cl-cookbook/numbers.html).

If you are looking for an exhaustive list of numerical operations defined by the
standard, then you can find that [here](http://clhs.lisp.se/Body/12_aa.htm)

### A More Idiomatic Approach

While this exercise encouraged the use of `=` in solving the final task (in
order to introduce the comparison operators), the standard provides a number of
predicates that can replace the expression you wrote with `mod` and `=`. The
equation from the final task might be more idiomatically written as:

```lisp
(zerop (mod (* pizzas 8) friends))
;; Instead of the more general
(= 0 (mod (* pizzas 8) friends))
```

Note that some more complex expressions, like testing if a number is even
(evenly divisible by 2) have been completely abstracted away.

```lisp
;; Using `mod` and `=`
(= 0 (mod x 2))
;; Using an equivalent, built-in function
(evenp x)
```

## Reference

```lisp
;; Addition
(+ 1 2 3 4 5) ; => 15

;; Subtraction
(- 15 3 2) ; => 10
(- 42)     ; => -42

;; Multiplication
(* 4 3 2 1) ; => 24

;; Division
(/ 64 16 2) ; => 2
(/ 5)       ; => 1/5

;; Exponentiation
(expt 2 8) ; => 256

;; Square Root
(sqrt 25) ; => 25

;; Modulo (Similar to remainder)
(mod 10 3) ; => 1

;; Equality
(= 2 2) ; => T
(= 3 5) ; => NIL

;; Rounding to the Nearest Whole Number
(round 3.14)  ; => 3
(round -3.14) ; => -3
(round 2.72)  ; => 3
(round -2.72) ; => -3

;; Rounding Towards Zero
(truncate 3.14)  ; => 3
(truncate -3.14) ; => -3
(truncate 2.72)  ; => 2
(truncate -2.72) ; => -2

;; Rounding Towards Negative Infinity
(floor 3.14)  ; => 3
(floor -3.14) ; => -4
(floor 2.72)  ; => 2
(floor -2.72) ; => -3

;; Rounding Towards Positive Infinity
(ceiling 3.14)  ; => 4
(ceiling -3.14) ; => -3
(ceiling 2.72)  ; => 3
(ceiling -2.72) ; => -2
```
