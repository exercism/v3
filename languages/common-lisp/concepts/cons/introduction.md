All Common Lisp code is either an "atom" (a single, indivisible value) or a list
(also termed a "cons"). A cons is made up of two parts: the first element and
the rest of the elements. For historical reasons these two parts are called the
`car` and the `cdr`. When these conses are evaluated as code, the first element
(`car`) represents the function being called while the rest of the elements
(`cdr`) represent the arguments to that function:

```lisp
(<function> <arg1> <arg2> ... <argN>)
; ^ car ^  |        ^ cdr ^
```