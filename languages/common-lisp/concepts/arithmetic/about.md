Common Lisp uses the standard arithmetic operators for most operations but is somewhat unique in using a "prefix-notation" as opposed to the more familiar "infix-notion". More visually:

```lisp
;; Infix-notation (non-lisp languages)
1 + 2 + 3 + 4 + 5 ; => 15
;; Prefix-notation (lisp languages)
(+ 1 2 3 4 5) ; => 15
```

While prefix notion turns some operations like `2 + 2` into the somewhat unfamiliar `(+ 2 2)` form, it makes it much easier to operate on more than one number at a time.

### Comparing Numbers (FIXME: Do I really belong here?)

Finally, you may find it useful to compare different numbers using functions like `=` (equal), `/=` (not equal to), and `>=` (greater than or equal to). When these comparisons are true (as in `(= 1 1)`), they return `T` and when they aren't (as in `(> 0 1)`), they return `NIL`.

### Arithmetic Operators With a Single Argument

As a small quirk, the `-` and `/` operators have a special meanings when applied to only one number:

```lisp
;; A single number passed to `-` is simply negated
(- 4)   ; => -4
(- -32) ; => 32
;; A single number passed to `/` returns the reciprocal
(/ 8)   ; => 1/8
(/ 0.1) ; => 10.0
```

### More Numerical Types & Operations (FIXME: Do I belong here either?!)

While [integers](concept://integers) and floats are the most common numerical types in Lisp, there also exists support for rational and complex numbers. These will likely be touched on in a future exercise, but to hold you over, you can take a peek at [The Common Lisp Cookbook](https://lispcookbook.github.io/cl-cookbook/numbers.html).

If you are looking for an exhaustive list of numerical operations defined by the standard, then you can find that [here](http://l1sp.org/cl/12.1.1)

### A More Idiomatic Approach

While this exercise encouraged the use of `=` in solving the final task (in order to introduce the comparison operators), the standard provides a number of predicates that can replace the expression you wrote with `mod` and `=`. The equation from the final task might be more idiomatically written as:

```lisp
(zerop (mod (* pizzas 8) friends))
;; Instead of the more general
(= 0 (mod (* pizzas 8) friends))
```

Note that some more complex expressions, like testing if a number is even (evenly divisible by 2) have been completely abstracted away.

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
