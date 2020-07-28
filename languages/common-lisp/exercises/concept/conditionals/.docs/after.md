## Summary

### Truth in Common Lisp

In Common Lisp, false values are represented by the empty list – `()` – or the symbol `nil`. These values can be quoted or unquoted.

All other values in Lisp represent truth. There also exists the special symbol
and variable `t` that is always true (can never be assigned to an empty list).

### Conditionals

Common Lisp provides the programmer with a number of different conditionals that
can be categorised by the number of "branches" they support.

#### Single-Branch Conditionals

The conditionals `when` and `unless` evaluate some code only when the provided
test is true or false respectively – evaluating to `nil` otherwise.

```lisp
(when (= 2 2) "All is well")      ; => "All is well"
```

#### The Two-Branch Conditional

The `if` conditional evaluates the first expression of the body when the test is
true and the second one otherwise.

```lisp
(if (= 2 2) 'how-honest 'you-liar) ; => HOW-HONEST
```

#### Many-Branch Conditionals

The Lisp "super-conditional" is `cond`, which can have an infinite number of
branches. Each branch has a test condition and body expression that are
surrounded by an extra pair of parentheses. If all of the tests evaluate to
false, then `nil` is returned.

```lisp
(cond ((= 0 2) 'nope)
      ((= 1 2) 'try-again)
      ((= 2 2) 'quite-true)
      ((= 3 2) 'too-far))
; => QUITE-TRUE
```

If you just want to test one value against a number of branches, you can use the
cleaner `case` expression. If none of the cases match, `nil` is returned. Both
`t` and `otherwise` can be used as catch-all cases

```lisp
(case 'elder-beast
  (cat "Meow")
  (bird "Chirp")
  (dog "Bark")
  (otherwise "???"))
; => "???"
```

## Diving Deeper

expressions, not statements

[Use this space to bring up any additional details regarding the topic and point
the student towards resources where they can learn more]

## Reference

```lisp
;; Equivalent False Values
()   ; => NIL
nil  ; => NIL
'()  ; => NIL
'nil ; => NIL

;; Some Different True Values
42                        ; => 42
"this statement is false" ; => "this statement is false"
t                         ; => T
't                        ; => T

;; Single-Branch Conditionals
(when (= 2 2) "All is well")      ; => "All is well"
(when (= 1 2) "All is well")      ; => NIL
(unless (= 2 2) "Time to panic!") ; => NIL
(unless (= 1 2) "Time to panic!") ; => "Time to panic!"

;; The Two-Branch Conditional
(if (= 2 2) 'how-honest 'you-liar) ; => HOW-HONEST
(if (= 1 2) 'how-honest 'you-liar) ; => YOU-LIAR

;; And so on...
```
