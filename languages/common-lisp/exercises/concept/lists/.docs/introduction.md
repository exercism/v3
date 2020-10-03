### Lists in Common Lisp

Given that the name of the language is Lisp which stands of _LISt Processing_ one might assume that the language has facilities for handling lists of items, and you'd be correct!

While Common Lisp has other data structures as well as lists, lists are still heavily used.

A list in Common Lisp is a sequence of items. The items themselves do not have to be the same type. For example you can have a list of `1`, `two`, `"III"`.

A list is composed of two parts called (for historical reason[1]) the `car` and the `cdr`. (Those are also the names of the functions used to access those two parts.).

A list is a recursive data structure in that both the `car` and `cdr` may themselves be lists. In fact a list of more a single item will have a `cdr` which itself has a `car` and `cdr` (see below). Typically the final `cdr` of a list will be `nil` but can be any value. Note: an empty list has `nil` for both `car` and `cdr`.

A list is represented by the values delimited with parentheses. (Thus Common Lisp source code can be seen to be a series of lists of things.)

### Creating Lists

One can simply type in a quoted list like this: `'(1, two, "III")` and that will cause a list to be created and evaluated (it evaluates to: `(1, two", "III")`.

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

### Length & random access

The length of a list can be determined by the use of `length`. An empty list has length zero.

An arbitrary item can be accessed with `nth` (note that lists are zero-indexed).

It is _not_ an error to request an index that is more than the length. Instead it evaluates to `nil`:

```lisp
(nth 23 '(short list))` ; => nil
```

There are also 10 helper methods for accessing the first 10 items of a list, they are named: `first`, `second`, `third`, `fourth`, `fifth`, `sixth`, `seventh`, `eighth`, `ninth`, and `tenth`.

### Checking for an item in a list.

A common way to check if an item is in a list is to use `member`. This function will evaluate to the `cdr` of the list which contains the item as its `car`. It will evaluate to `nil` if the item was not found.

### Sub-lists

A sub-list of a list can be retrieved by the use of `subseq`. This function takes a list and two indexes: the start and end index of the desired range. This range is inclusive of the starting index but non-inclusive of the ending index. The end index argument is optional and defaults to the length of the list. Note that providing starting or ending indexes which are invalid (zero, negative or greater than the length of the list) will result in an error.

```lisp
(subseq '(0 1 2 3 4 5) 1 4) ; => (1 2 3)
```

### Reversing lists

A list can be reversed in order by the function `reverse` which does what it says. This is _not_ a destructive operation. `reverse` evaluates to a _new_ list which in which all the items from the original list are in reverse order.
