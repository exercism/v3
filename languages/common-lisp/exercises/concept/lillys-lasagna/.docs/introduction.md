## functions

Common Lisp provides the `defun` expression to allow one to create
global functions.

To define a global function in Common Lisp one uses the `defun`
expression. This expression takes as its first argument a list or
parameters (and empty list of the function has no parameters). This is
followed by an optional documentation string (see below), then zero or
more expressions which make up the "body" of the function.

Functions may have zero or more parameters.

```lisp
(defun no-args () (+ 1 1))

(defun add-one (x) (+1 x))

(defun add-nums (x y) (+ x y))
```

Calling a function is done by evaluating an expression with the symbol
designating the function as the first element of the expression with
the arguments to the function (if any) as the remaining items in the
expression.

The value that a function evaluates to is the value of the last
expression in the function body that was evaluated. All functions
evaluate to a value.

```lisp
(add-nums 2 2) ;; => 4
```

Functions can also have, optionally, a documentation string (also
called a 'docstring'). If provided it comes after the argument list
but before the body of the function. The documentation string can be
accessed via `documentation`.

```lisp
(defun add-nums (x y) "Add X and Y together" (+ x y))

(documentation 'add-nums 'function) ;; => "Add X and Y together"

;; Not that if one provides a docstring but fails to provide a body
;; then the docstring is interpreted by Common Lisp as the body, not
;; the docstring
(defun no-body ())
(no-body) ;; => NIL

(defun mistake () "This is not a docstring")
(mistake) ;; => "This is not a docstring"
(documentation 'mistake 'function) ;; => NIL
```
