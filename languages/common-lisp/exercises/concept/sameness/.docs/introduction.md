Common Lisp has many different equality predicates. This differs from other programming languages which may have only one or two (perhaps `==` and `===` for example). Some of these predicates in Common Lisp are specific to types (such as `string=` for checking equality of strings and `char=` for checking equality of characters) while others are generic. It is these latter that this exercise will teach.

There are four generic equality predicates and they differ by their restrictiveness on what they consider "equal". They are, in order from most restrictive to least restrictive: `eq`, `eql`, `equal`, and `equalp`. 

A quick set of definitions (leaving out a few details) are as follows:

* `eq`: defines equality as meaning the two objects are identical.
* `eql`: defines equality as meaning two numbers with the same type and value, two characters which are the same, or for anything else if they are `eq`. 
* `equal`: defines equality as meaning: two lists are `equal` if each element is also `equal`; two arrays are `equal` if each element is `eq`; two strings are `equal` if each element is `eql`; everything else is `equal` if they are `eql`.
* `equalp`: defines equality as meaning: strings and characters are compared in a case-insensitive manner; numbers are compared with some type conversion; lists and arrays are `equalp` if every element is also `equalp`
