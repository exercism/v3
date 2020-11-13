## Reference

```lisp
;; Returning a symbol from a `defun`
(defun gimme-foo () 'foo)
(gimme-foo) ; => FOO

;; The same, but as a keyword
(defun gimme-bar () :bar)
(gimme-bar) ; => :BAR
```