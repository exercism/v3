## Summary

### Truth in Common Lisp

In Common Lisp, false values are represented by the empty list – `()` – or the
symbol `nil`. These values can be quoted or unquoted.

All other values in Lisp represent truth. There also exists the special constant
symbol `t` that is always equal to `t` (and is therefore always true).

### Conditionals

Common Lisp provides the programmer with several different conditionals that can
be categorised by the number of "branches" they support.

#### Single-Branch Conditionals

The conditionals `when` and `unless` evaluate some code only when the provided
test is true or false respectively – evaluating to `nil` otherwise.

```lisp
(when (= 2 2) "All is well")      ; => "All is well"
(unless (= 2 2) "Time to panic!") ; => NIL
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
      ((= 3 2) 'too-far)
      (t 'something-else))
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

### All Conditionals Are Expressions

Unlike many other programming languages, all conditionals in Common Lisp are
_expressions_ not statements. This means that all Lisp conditionals evaluate to
some value and can be substituted for concrete parameters.

As an example:

```lisp
;; Doubles x when it's even
(* x (if (evenp x) 2 1))
```

### The Stealth Conditionals

The boolean `and` and `or` operations in Common Lisp are short-circuiting macros
that can be used to reduce the duplication of certain conditional
expressions.

The `and` macro will immediately return `nil` if a single false value is
encountered, but the last true value of the `and` otherwise:

```lisp
(and 42 "Magic" :cool) ; => :COOL
(and () "Magic" :cool) ; => NIL
```

The `or` macro returns the first true value it encounters or `nil` if there were
no true values:

```lisp
(or () 42 nil) ; => 42
(or () NIL nil) ; => NIL
```

### I'm Exhausted...

As mentioned previously, when none of the branches in a `case` statement match,
and there is no `otherwise` clause, `nil` is returned. Occasionally, however, a
failure to match any branch should be treated as an error – this is where
`ecase` (for **exhaustive** matching) comes in.

It's used in exactly the same way as `case`, but signals an error instead of
returning `nil` when there is no match.

```lisp
(case 'elder-beast
  (cat "Meow")
  (bird "Chirp")
  (dog "Bark"))
; => NIL

(ecase 'elder-beast
  (cat "Meow")
  (bird "Chirp")
  (dog "Bark"))
; => ERROR: ELDER-BEAST fell through ECASE expression. Wanted one of (CAT BIRD DOG).
```

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

;; Multi-Branch Conditionals
(cond ((= 0 2) 'nope)
      ((= 1 2) 'try-again)
      ((= 2 2) 'quite-true)
      ((= 3 2) 'too-far)
      (t 'something-else))
; => QUITE-TRUE

(case 'elder-beast
  (cat "Meow")
  (bird "Chirp")
  (dog "Bark")
  (otherwise "???"))
; => "???"
```
