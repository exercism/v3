In Common Lisp, global, named functions are defined with `defun`.

This form takes as its first argument a list of parameters for the
function being defined. After that it can, optionally, take a string
for use as documentation to that function. Finally there are zero or
more expressions which make up the body of the function.

Calling a function is done by using the name of the function as the
first element of an expression, followed by any parameters to that
function.

```lisp
(defun add-one (x) (1+ x))

(add-one 3) ;; => 4
```

Functions can also take [optional
parameters](concept://default-parameters), [keyword
parameters](concept://named-parameters), [rest
parameters](concept://rest-parameters). These will be discussed in
later concepts.

The name of the function is in [scope](concept://scope) in the body of
the function so that [recursion](concept://recursion) can be
performed.

```lisp
(defun countdown (x)
  (if (zerop x) (print 'BLAST-OFF)
    (progn (print x) (countdown (1- x)))))

(countdown 3) ;; will print "3","2", "1" and "BLAST-OFF" to
              ;; standard output before evalutaing to 'BLAST-OFF
```
