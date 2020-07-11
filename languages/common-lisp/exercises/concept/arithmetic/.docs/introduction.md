## Numerical Types

In Common Lisp, like many languages, numbers come in a couple of types. This
exercise focuses on getting to grips with two of the most basic number-types:

- Integers: whole numbers without a decimal point (like `-6`, `0`, `25`, `1234`,
  etc.)
- Floating-Point Numbers: fractional or whole numbers including a decimal point (like
  `3.14`, `-1.7`, `99.99`, `2048.0`)

In general, if you are working with only whole numbers, you should prefer
integers as they don't suffer from the same loss of precision as floating-point
numbers do over many calculations.

## Arithmetic & Prefix Notation

Common Lisp uses the standard arithmetic operators for most operations (you can
find a [full list here](http://clhs.lisp.se/Body/12_aa.htm)), but is somewhat
unique in using a "prefix-notation" as opposed to the more familiar
"infix-notion". More visually:

```lisp
;; Infix-notation (non-lisp languages)
1 + 2 + 3 + 4 + 5 ; => 15
;; Prefix-notation (lisp languages)
(+ 1 2 3 4 5) ; => 15
```

While prefix notion turns some operations like `2 + 2` into the somewhat
unfamiliar `(+ 2 2)` form, it makes it much easier to operate on more than one
number at a time.

As a final quirk, the `-` and `/` operators have a special meanings when applied
to only one number:

```lisp
;; A single number passed to `-` is simply negated
(- 4)   ; => -4
(- -32) ; => 32
;; A single number passed to `/` returns the reciprocal
(/ 8)   ; => 1/8
(/ 0.1) ; => 10.0
```

## Comparing Numbers

Finally, you may find it useful to compare different numbers using functions
like `=`, `/=` (not equal to), and `>=` (greater than or equal to). When these
comparisons are true (as in `(= 1 1)`), they return `T` and when they aren't (as
in `(> 0 1)`), they return `NIL`. For a full list of comparison operators, see
[here](http://www.lispworks.com/documentation/HyperSpec/Body/f_eq_sle.htm).
