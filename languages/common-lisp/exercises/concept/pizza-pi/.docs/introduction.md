In Common Lisp, like many languages, numbers come in a few of types – two of the most basic are:

## integers

Like many languages Common Lisp contains integers. These are whole numbers without a decimal point (like `-6`, `0`, `25`, `1234`,
  etc.)

Common Lisp defines no limits on the magnitude of integers. Integers can be arbitrarily large (or small if negative).

In general, if you are working with only whole numbers, you should prefer
integers as they don't suffer from the same loss of precision as floating-point
numbers do over many calculations.

## floating-point-numbers

Also like many languages, Common Lisp contains floating point numbers. These are fractional or whole numbers including a decimal point (like `3.14`, `-1.7`, `99.99`, `2048.0`)

## arithmetic

Common Lisp uses the standard arithmetic operators for most operations but is
somewhat unique in using a "prefix-notation" as opposed to the more familiar
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

## Comparing Numbers

Finally, you may find it useful to compare different numbers using functions
like `=` (equal), `/=` (not equal to), and `>=` (greater than or equal to). When
these comparisons are true (as in `(= 1 1)`), they return `T` and when they
aren't (as in `(> 0 1)`), they return `NIL`.
