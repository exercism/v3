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