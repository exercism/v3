Common Lisp uses the standard arithmetic operators for most operations but is somewhat unique in using a "prefix-notation" as opposed to the more familiar "infix-notion". More visually:

```lisp
;; Infix-notation (non-lisp languages)
1 + 2 + 3 + 4 + 5 ; => 15
;; Prefix-notation (lisp languages)
(+ 1 2 3 4 5) ; => 15
```

While prefix notion turns some operations like `2 + 2` into the somewhat unfamiliar `(+ 2 2)` form, it makes it much easier to operate on more than one number at a time.

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

### Comparing Numbers (FIXME: Do I really belong here?)

Finally, you may find it useful to compare different numbers using functions like `=` (equal), `/=` (not equal to), and `>=` (greater than or equal to). When these comparisons are true (as in `(= 1 1)`), they return `T` and when they aren't (as in `(> 0 1)`), they return `NIL`.

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
```
