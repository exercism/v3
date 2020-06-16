# Concepts of Roman Numerals

This one is a bit of 'cheat' of an exericse - but the concept is real.

## Concepts

- `format`
  - `format-numbers`

## Commentary

This exercise is of a sub-concept of the larger concept of `format`.
The concept of `format` is big and will be split into sub-concepts
according to the types of things you can do with `format` directives.

- `format-basic`: Things like A , W, and S, meaning of the stream
  argument (perhaps deferring anything other than `nil` or `t` for
  when streams are discussed. This concept could include discussion of
  `fresh-line` and `new-line`. With this concept a student could do
  useful things with `format`.
- `format-numbers`: Number printing options (including radix control
  and floating point numbers)
- `format-language`: Case conversions, plurals.
- `format-tables`: Justification and tables
- `format-iteration`: Iteration and jumping arguments with `~*` and
  friends
- `format-conditional`: Conditional printing options
- `format-misc`: a collection of any other format directive that did
  not fit into any others.

This exercise's concept of `format-numbers` should depend upon the
concept of `format-basic` as the `nil` vs. `t` as stream argument will
need to be addressed (or stubbed for the student).
