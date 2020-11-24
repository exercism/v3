TODO: Add info about single and double-floats (as well as short and long?)

Floating-Point Numbers are fractional or whole numbers including a decimal point (like `3.14`, `-1.7`, `99.99`, `2048.0`)

In general, if you are working with only whole numbers, you should prefer integers as they don't suffer from the same loss of precision as floating-point numbers do over many calculations.

## Reference

```lisp
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
