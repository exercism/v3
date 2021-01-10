Common lisp provides several different conditional expressions, the main difference being the number of branches they support.

- `when` and `unless` allow for a single branch:

```lisp
(when (= 2 2) "All is well")      ; => "All is well"
(unless (= 2 2) "Time to panic!") ; => NIL
```

- `if` provides the classic if-then-else construct:

```lisp
(if (= 2 2) 'how-honest 'you-liar) ; => HOW-HONEST
```

- `cond` provides a way to have multiple branches without nesting `if` expressions:

```lisp
(cond ((= 0 2) 'nope)
      ((= 1 2) 'try-again)
      ((= 2 2) 'quite-true)
      ((= 3 2) 'too-far)
      (t 'something-else))
; => QUITE-TRUE
```

- `case` provides a classic 'switch' style construct: It checks a single value against a number of branches:

```lisp
(case 'elder-beast
  (cat "Meow")
  (bird "Chirp")
  (dog "Bark")
  (otherwise "???"))
; => "???"
```
