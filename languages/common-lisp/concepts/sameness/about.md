Sameness is the concept about equality of things.

Common Lisp, like other languages has a set of rules on how to decide if two objects are the 'same'. These rules define four levels, each with a function that performs that level of checking. The levels are structured from strictest to loosest.

### `eq`

The first level is object identity. This equality is checked with the function [`eq`][hyper-eq]. The two objects being checked for equality must be the very same object:

```lisp
(eq 'apples 'apples) ; => T
(eq 'apples 'oranges) ; => NIL

(eq '(a b c) '(a b c) ; => NIL (these two lists have the same contents but are not the same list)
(let ((list1 '(a b c)) (list2 list1)) (eq list1 list2)) ; => T (these two lists are the same list)
```

### `eql`

The second level adds equality of numbers and characters. This equality is checked with the function [`eql`][hyper-eql]. The way the checking is done depends upon the types of the arguments:

- Any two objects which are `eq` are `eql`
- Numbers are `eql` if they are of the same type and value
- Characters are `eql` if they they represent the same character.

```lisp
(eql 1 1) ; => T
(eql 1 1/1) ; => NIL (one number is an integer, the other a rational)
(eql #\c #\c) ; => T
(eql #\c #\C) ; => NIL (case is different)
```

One may wonder why numbers and characters are not compared for object identity with [`eq`][hyper-eq]. The Common Lisp standard allows implementations to copy numbers and characters if they choose to do so. Thus `0` and `0` may not be [`eq`][hyper-eq] as they may be different instances of the number `0`.

### `equal`

The third level checks for structural similarity. This equality is checked with [`equal`][hyper-equal]. The way the checking is done depends upon the types of the arguments:

- symbols are compared as if with [`eq`][hyper-eq]
- characters, and numbers are compared as if with `eql`
- conses are [`equal`][hyper-equal] if their elements are [`equal`][hyper-equal]. This is done recursively.
- strings and bit vectors are [`equal`][hyper-equal] if their elements are `eql`
- arrays of other types are compared as if with [`eq`][hyper-eq]
- pathnames are [`equal`][hyper-equal] if they are functionality equivalent. (There is room for implementation dependendant behavior here with regards to case sensitivity of the strings which make up the components of the pathnames.)
- objects of any other type are compared as if with [`eq`][hyper-eq]

```lisp
(equal '(a (b c)) '(a (b c))) ; => T (conses are equal if their contents are equal)
(equal "hello" "hello") ; => T
(equal "hello" "HELLO") ; => NIL
(equal #(1 2 3) #(1 2 3)) ; => NIL (arrays are equal only if eq)
(equal #P"foo/bar.md" #P"foo/bar.md") ; => T (pathnames are equal if "functionally equivalent"
```

### `equalp`

The fourth and most loose level of equality is checked with [`equalp`][hyper-equalp]. The how the checking is done depends upon the types:

- if the two objects are [`equalp`][hyper-equalp] then they are [`equalp`][hyper-equalp]
- numbers are [`equalp`][hyper-equalp] if they have the same vaule even if they are not of the same type
- characters and strings are compared case-insensitively
- conses are [`equalp`][hyper-equalp] if their elements are [`equalp`][hyper-equalp]. This is done recursively.
- arrays are [`equalp`][hyper-equalp] if they have the same number of dimensions, those dimensions are the same, and each element is [`equalp`][hyper-equalp].
- structures are [`equalp`][hyper-equalp] if they have the same class and slots and each of those slots are [`equalp`][hyper-equalp] between the two structures.
- hash tables are [`equalp`][hyper-equalp] if they both have the same `:test` function, they have the same keys (as compared with that `:test` function) and that those keys have the same values as compared with [`equalp`][hyper-equalp].

```lisp
(equalp 1 1.0) ; => T
(equalp #\c #\C) ; => T
(equalp "hello" "HELLO") ; => T
(equalp #(1 2 3) #(1.0 2.0 3.0)) ; => T
(equal #S(TEST :SLOT1 'a :SLOT2 'b) #S(TEST :SLOT1 'a :SLOT2 'b)) ; => T
```

### Type-specific functions

The above are the 'generic' equality functions. They work, as defined, for any type. This can be useful when one writes generic code that does not know the types of objects it will be comparing until run-time. However it is generally considered "better style" to use type specific equality functions when one knows the types being compared. For example `string=` rather than `equal`. These functions will be presented and discussed in later concepts.

[hyper-eq]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eq.htm
[hyper-eql]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eql.htm
[hyper-equal]: http://www.lispworks.com/documentation/HyperSpec/Body/f_equal.htm
[hyper-equalp]: http://www.lispworks.com/documentation/HyperSpec/Body/f_equalp.htm
