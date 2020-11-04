### Lists

Given that the name of the language is Lisp which stands of _LISt Processing_ one might assume that the language has facilities for handling lists of items, and you'd be correct!

While Common Lisp has other data structures as well as lists, lists are still heavily used.

A list in Common Lisp is a sequence of items. The items themselves do not have to be the same type. For example you can have a list of `1`, `two`, `"III"`.

#### Creating Lists

One can simply type in a quoted list like this: `'(1 two "III")` and that will cause a list to be created and evaluated (it evaluates to: `(1 two "III")`.

There are also two main functions used to create lists: `list` and `cons`.

`list` takes zero or more arguments and evaluates to a list created with those values:

```lisp
(list 1 'two "III") ; => (1 two "III")
```

`cons` takes two items and creates a list which has as its `car` the first item and as its `cdr` the second item:

```lisp
(cons 1 2)       ; => (1 . 2) ;; (a list without `nil` as its `cdr` is printed in this way.)
(cons 1 nil)     ; => (1)
(cons 1 (cons 2 nil)) ; => (1 2)
```

`car` and `cdr` can be used to access the `car` and `cdr` respectively.

(`first` and `rest` are synonyms of `car` and `cdr` and work exactly the same.)

#### Length & Random Access

The length of a list can be determined by the use of `length`. An empty list has length zero.

An arbitrary item can be accessed with `nth` (note that lists are zero-indexed).

It is _not_ an error to request an index that is more than the length. Instead it evaluates to `nil`:

```lisp
(nth 23 '(short list))` ; => nil
```

There are also 10 helper methods for accessing the first 10 items of a list, they are named: `first`, `second`, `third`, `fourth`, `fifth`, `sixth`, `seventh`, `eighth`, `ninth`, and `tenth`.
