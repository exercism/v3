## Quoting

In the introduction, quoting `FOO` makes it act a lot like a keyword, but unlike
keywords (which can only be symbols), all S-Expressions can be quoted:

```lisp
;; This line is evaluated as code
(gimme-foo)  ; => FOO

;; This line is treated as data
'(gimme-foo) ; => (GIMME-FOO)
```

In case you didn't see the hints, here is an excellent [Stack Overflow
Answer][so-quoting] discussing quoting in a bit more detail.

## Reference

```lisp
;; Returning a symbol from a `defun`
(defun gimme-foo () 'foo)
(gimme-foo) ; => FOO

;; The same, but as a keyword
(defun gimme-bar () :bar)
(gimme-bar) ; => :BAR
```

[so-quoting]: https://stackoverflow.com/questions/134887/when-to-use-or-quote-in-lisp