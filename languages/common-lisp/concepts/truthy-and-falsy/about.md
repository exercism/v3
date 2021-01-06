In Common Lisp, false values are represented by the empty list – `()` – or the
symbol `nil`. These values can be quoted or unquoted.

```lisp
;; Equivalent False Values
()   ; => NIL
nil  ; => NIL
'()  ; => NIL
'nil ; => NIL
```

All other values in Lisp represent truth. There also exists the special constant
symbol `t` that is always equal to `t` (and is therefore always true).

```lisp
;; Some Different True Values
42                        ; => 42
"this statement is false" ; => "this statement is false"
t                         ; => T
't                        ; => T
```
