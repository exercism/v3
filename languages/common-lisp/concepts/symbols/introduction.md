There are a couple of things to note regarding the example above. Firstly, the
_symbol_ `FOO` is an atom, as it only has one "part" (unlike a cons which has
two). Additionally, Common Lisp is **case-insensitive**, so symbols are often
returned as all uppercase, but `foo`, `Foo` and `FOO` are equivalent.

Symbols in Lisp are special values that can point to other values or, in the
case of _keywords_, themselves. When symbols are evaluated by Lisp, they are
replaced with the values they point to:

```lisp
foo  ; => <whatever-foo-points-to>
:foo ; => :FOO
```

Note that keywords are denoted by a leading colon (`:`).

Quoting – the addition of `'` before an S-expression – tells Lisp to not
evaluate that expression. By quoting `'foo` in our `defun` example, we avoided
Lisp attempting to look up (and failing to find) whatever `FOO` was supposed to
point to, instead, returning the value `FOO` itself. If `FOO` has not been
defined anywhere in our program:

```lisp
foo  ; => <ERROR! Lisp doesn't know what foo points to!>
'foo ; => FOO
```

For now, you can consider this just as a way to return symbols from a function,
but we will revisit quoting and further explore its implications in future
concept exercises.
