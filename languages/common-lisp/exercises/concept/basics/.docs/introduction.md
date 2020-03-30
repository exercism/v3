## Comments

As a quick side-note before jumping into things, Common Lisp, like most programming languages, allows the programmer to write "comments" that are ignored when your program is being run. In Common Lisp, single-line comments begin with one or more semi-colons (`;`) and follow a couple of conventions:

```lisp
;;;; A short title for a block of code
;;; A longer description of a block of code
;; Describe the code below this line
; Describe the code before the semi-colon (;) on this line
```

Multi-line comments are possible using `#|` and `|#`:

```lisp
#|
This entire block of text is a comment
and can span several lines!

How cool is that?!
|#
```

Occasionally, you may see the following:

```lisp
(code...) ; => value
```

Where the comment is being used to indicate what value is returned by Common Lisp after running the code on that line.

## S-Expressions

All Common Lisp code is made from S-Expressions (or sexpr for short). An S-Expression is either an atom or a list (also termed a "cons"). A cons is made up of two parts: the first element and the rest of the elements. For historical reasons these two parts are called the `car` and the `cdr`. When S-Expressions are evaluated as code, the first element (`car`) represents the function being called while the rest of the elements (`cdr`) represent the arguments to that function. In other words:

```lisp
(<function> <arg1> <arg2> ... <argN>)
; ^ car ^  |        ^ cdr ^
```

When S-Expressions like this are evaluated, they automatically return some value which takes the place of this expression. When writing your own functions (using `defun`), the last value within the body of the `defun` is automatically returned. For example, if you define the following function:

```lisp
(defun gimme-foo () 'foo)
```

Then call the function as an S-Expression (note that `gimme-foo` has no arguments):

```lisp
(gimme-foo)
```

Lisp evaluates this to:

```lisp
'foo
```

Functions and the use of `defun` will be discussed in much greater detail in later concept exercises.

## Symbols and Quoting

There are a couple of things to explain regarding the example above; for example, what is `foo`? The _symbol_ `foo` is an example of an atom, as it only has one "part" (unlike a cons which has two). Symbols in Lisp are special names that can point to other values or, in the case of _keywords_, themselves. When symbols are evaluated by Lisp, they are replaced with the values they point to. For example:

```lisp
foo  ; => <whatever-foo-points-to>
:foo ; => :foo

```

Note that keywords are denoted by a leading colon (`:`).

Given that all code in Common Lisp is composed of S-Expressions, what stops Lisp from trying to evaluate _everything_ in your program? Why didn't Lisp try to evaluate `'foo` from the `defun` example above? The answer comes in the form of "quoting".

Quoting – the addition of `'` before an S-expression – tells Lisp to not evaluate that expression. By quoting `'foo`, we avoided Lisp attempting to look up (and failing to find) whatever `foo` was supposed to point to, instead, returning the value `'foo` itself.

## Truthy and Falsy Values

The last important thing to know for this exercise is that Common Lisp doesn't have a special boolean type for expressing "true" and "false"; rather, the special symbol `nil` and the empty list / cons `'()` are regarded as "false" values and all other values are "true". Conventionally, however, functions in Common Lisp will return the symbols `nil` for false and `t` for true.
