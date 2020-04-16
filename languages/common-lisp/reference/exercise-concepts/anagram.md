# Concepts of Anagram

While there are, of course, different ways to attack this problem the
'canonical' way is to write a function that "normalizes" the
candidates and the input word so that they can then be easily
compared. It is this method which was reviewed for concepts. (ref. the
[exercism v2 common-lisp track exercise][v2-exercise].)

## Concepts

- Sequence Functions: including filtering and sorting
- [Strings][strings]
- [Functions][functions]: it is likely the student will create a
  helper function for the work.
- [Higher-order functions][hof]: used when calling functions such as
  `sort` or `remove-if`.
- [Sameness][sameness]: `string=`, `char=` etc.

## Commentary

Looking through the [concepts][concepts] & [types][types] it does not
look like there is a good fit for 'sequence functions'. By this
concept I mean those functions such as `copy-seq`, `find`, `remove-if`
etc. which operate on all types of sequences. Perhaps this track will
have a type concept of `sequence` which is a super-concept of types
such as `list`, `array` etc..

[v2-exercise]: https://github.com/exercism/common-lisp/blob/master/exercises/anagram/example.lisp
[concepts]: ../../../../reference/concepts
[types]: ../../../../reference/types
[strings]: ../../../../reference/types/string.md
[functions]: ../../../../reference/concepts/functions.md
[hof]: ../../../../reference/concepts/higher_order_functions.md
[sameness]: ../../../../reference/concepts/sameness.md
