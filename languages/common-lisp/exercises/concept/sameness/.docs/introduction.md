Common Lisp has many different equality predicates. This differs from other programming languages which may have only one or two (perhaps `==` and `===` for example). Some of these predicates in Common Lisp are specific to types, while others are generic. It is these latter that this exercise will teach.

There are four generic equality predicates and they differ by their restrictiveness on what they consider "equal". They are, in order from most restrictive to least restrictive: `eq`, `eql`, `equal`, and `equalp`.

A quick set of definitions (leaving out a few details) are as follows:

- `eq`: defines equality as meaning the two objects are identical.
  _e.g._:

  - `(eq 'a 'a) ; => T`
  - `(eq (list 1 2) (list 1 2)) ; => NIL`

- `eql`: defines equality as meaning two numbers with the same type and value, two characters which are the same,
  or for anything else if they are `eq`.
  _e.g._:

  - `(eql 1 1) ; => T`
  - `(eql #\a #\a) ; => T`
  - `(eql 1 1.0) ; => NIL`
  - `(eql #\a #\A) ; => NIL`

- `equal`: defines equality as meaning: two lists are `equal` if each element is also `equal`; two arrays are `equal` if each element is `eq`; two strings are `equal` if each element is `eql`; everything else is `equal` if they are `eql`.
  _e.g._:

  - `(equal (list 1 2) (list 1 2)) ; => T`
  - `(equal #(1 2) #(1 2)) ; => T`
  - `(equal "foo" "foo") ; => T`

- `equalp`: defines equality as meaning: strings and characters are compared in a case-insensitive manner; numbers are compared with some type conversion; lists and arrays are `equalp` if every element is also `equalp`, structures if they are the same type and all slots are `equalp` and hash-tables if their keys and values are `equalp`
  _e.g._:
  - `(equal "foo" "FoO") ; => T`
  - `(equal 3 3.0) ; => T`
  - `(equal (make-a-structure :slot1 1 :slot2 2) (make-a-structure :slot1 1 :slot2 2)) ; => T`
