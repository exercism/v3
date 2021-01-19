All Common Lisp code is made of S-Expressions (Symbolic Expressions). They are called sexprs for short. Every sexpr is either an atom or a cons. When S-Expressions are evaluated, they automatically return some value which takes the place of the expression. When writing your own functions (using `defun`), the last value within the body of the `defun` is automatically returned:

```lisp
;; Defining a new function
(defun gimme-foo () 'foo)
;; Calling the function as an S-Expression
(gimme-foo) ; => FOO
```
