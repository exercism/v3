TODO: Discuss fixnum and bignum, mention the size limits of fixnums (2^32, 2^64?)

Integers are whole numbers without a decimal point (like `-6`, `0`, `25`, `1234`, etc.)

In Common Lisp, integers can be arbitrarily large or small, so there is no need to worry about "overflowing" when working with them. While some other languages might experience a loss of precision or wrap large numbers, the size of integers in Common Lisp is limited only by the amount of memory available.

```lisp
;; Big integers are no problem
(expt 2 200) ; => 1606938044258990275541962092341162602522202993782792835301376
```
