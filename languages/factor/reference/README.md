# Factor reference

## Concepts

The Factor concept exercises are based on concepts. The list below contains the concepts that have been identified for the Factor language.

### Concatenative

The core features of concatenative language which a Factor developer should know about are:

- [Stack paradigm basics](../../../reference/concepts/stack_paradigm_basics.md)
- [Words and vocabularies](../../../reference/concepts/words_and_vocabularies.md)

### Object System

The core object features a Factor developer should know about are:

- [Classes and objects](../../../reference/concepts/classes_and_objects.md)
- [Accessors](../../../reference/concepts/accessors.md)
- [Generic methods](../../../reference/concepts/methods.md)
- [State](../../../reference/concepts/state.md)
- [Mutation](../../../reference/concepts/mutation.md)
- [Inheritance](../../../reference/concepts/inheritance.md)

### Dynamic typing

The core type system features a Factor developer should know about are:

- [Type vs Class](../../../reference/concepts/type_vs_class.md)
- [Gradual type checking](../../../reference/concepts/gradual_type_checking.md)
- [Type conversion](../../../reference/concepts/type_conversion.md)
- [Underlying types](../../../reference/concepts/underlying_types.md)
- [Type reflection](../../../reference/concepts/type_reflection.md)

### Functional

Factor relies on anonymous functions, called quotations, to generalise procedures.

- [Quotations](../../../reference/concepts/quotations.md)
- [Recursion](../../../reference/concepts/recursion.md)
- [Immutability](../../../reference/concepts/immutability.md)

### General

- Arithmetic overflow
- [Sameness](../../../reference/concepts/sameness.md)
- [Conditionals](../../../reference/concepts/conditionals.md)
- [Enumeration](../../../reference/concepts/enumeration.md)
- Exception handling
- Implicit/explicit conversion
- Threading
- Side effects
- Unsafe code
- Generalised reflection

## Concept interpretation

The concept exercises use the following concepts:

| concept                  | interpretation                                                                                                                                                                                                                                                                                                          |
| ------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `basic-numbers`          | Know of the existence of the two most commonly used number classes, `integer`, `ratio` and `float`, and understand that they represent whole numbers,  rational numbers, and floating-point numbers respectively. Know of basic operators such as multiplication and comparison. Know where it's documented, or at least how to search for it.      |
| `basic-strings`          | Know of the existence of the `string` class. Know of some basic functions (like looking up a character at a position, or slicing the string). Know how to do basic string formatting. Know where it's documented, or at least how to search for it.                                                                      |
| `basic-dates`            | Know of the existence of the `calendar.timestamp` class. Know of the individual, calendar-related properties. Know how to access the current date. Know how to compare dates. Know how to convert a `string` to a `timestamp` and vice versa. Know where it's documented, or at least how to search for it                              |
| `basic-sequences`        | Know of the existence of the `sequence` mixin class, the _sequence protocol_, and how to iterate over a sequence.                                                                                                                                                                                                                                                                                  |
| `basic-assocs`           | Know of the existence of the `assoc` mixin class for associative sequences, the _assoc protocol_, and how to iterate over an assoc.                                                                                                                                                                                                                                                                                  |
| `basic-hashtables`       | Know of the existence of the `hashtable` class, the _assoc protocol_, and how to iterate over a hashtable.                                                                                                                                                                                                                                                                                  |
| `basic-enums`            | Know of the existence of the `enum` keyword. Know how to define enum members. Know how to convert a `string` to an `enum` and vice versa. Know where it's documented, or at least how to search for it                                                                                                                  |
| `advanced-enums`         | Know how to define a "flags" enum. Know how to add, remove or check for flags.                                                                                                                                                                                                                                          |
| `basic-time`             | Know of the existence of the `DateTime` type. Know of the individual, time-related properties. Know where it's documented, or at least how to search for it.                                                                                                                                                            |
| `basic-class-conversion` | Know that it is sometimes possible to convert between classes.                                                                                                                                                                                                                                            |
| `basic-quotations`       | Know of the existence of quotations.                                                                                                                                                                                                                                 |
| `floating-point-numbers` | Know the difference between the `ratio` and `float` classes. Know when each type appears in a program and when to use each.                                                                                                                                                                                                   |
| `string-formatting`      | Know how to format a string. Know where it's documented, or at least how to search for it.                                                                                                                                                                                                                              |
| `bitwise-operations`     | Know how to apply bitwise operations to numbers. Know where it's documented, or at least how to search for it.                                                                                                                                                                                                          |
| `basic-combinators`      | Know how to use basic combinator words to apply higher-order functions (quotations), and how to define your own. |
| `conditional-combinators` | Know how to use combinators to perform conditional logic with quotations, and how to define your own. |
| `sequence-combinators`   | Know how to use combinators to operate on sequences with quotations, and how to define your own. |
| `assoc-combinators`      | Know how to use combinators to operate on assocs with quotations, and how to define your own. |
| `dataflow-combinators`   | Know how to use dataflow combinators to control the flow of data with quotations, and how to define your own. |
| `looping-combinators`    | Know how to use looping combinators to perform iteration with quotations, and how to define your own. |
| `compositional-combinators` | Know how to use compositional combinators to compose quotations, and how to define your own. |
| `short-circuit-combinators` | Know how to use short-circuit combinators to short-circuit on quotation outputs, and how to define your own. |

This also indicates that for example `basic-strings` does **not** teach using custom formatting strings and that `basic-numbers` does **not** teach about checked/unchecked arithmetic.

## Reference docs

Reference docs are written to help explain a particular Factor concept to a student when no appropriate online document can be found. They will be used when creating exercises and as references in exercise documentation.

The following reference docs have been written:


## TODO

- Reference doc on difference between `static readonly` and `const`

[issues-improve-reference]: https://github.com/exercism/v3/issues?q=is%3Aissue+is%3Aopen+label%3Atrack%2Ffactor+label%3Atype%2Fimprove-reference+label%3Astatus%2Fhelp-wanted
[issues-new-reference]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Atrack%2Ffactor+label%3Atype%2Fnew-reference+label%3Astatus%2Fhelp-wanted+
[code_style]: ./code_style.md
[memory_allocation]: ./memory_allocation.md
[bool]: ../../../reference/types/boolean.md
[string]: ../../../reference/types/string.md
[char]: ../../../reference/types/char.md
[null]: ../../../reference/types/null.md
[int]: ../../../reference/types/integer.md
[uint]: ../../../reference/types/integer.md
[byte]: ../../../reference/types/byte.md
[sbyte]: ../../../reference/types/byte.md
[short]: ../../../reference/types/short.md
[ushort]: ../../../reference/types/short.md
[long]: ../../../reference/types/long.md
[ulong]: ../../../reference/types/long.md
[double]: ../../../reference/types/double.md
[float]: ../../../reference/types/single.md
[decimal]: ../../../reference/types/decimal_number.md
[big-integer]: ../../../reference/types/big_integer.md
[array]: ../../../reference/types/array.md
[list]: ../../../reference/types/list.md
[map]: ../../../reference/types/map.md
[set]: ../../../reference/types/set.md
[stack]: ../../../reference/types/stack.md
[queue]: ../../../reference/types/deque.md
[class]: ../../../reference/types/class.md
[struct]: ../../../reference/types/struct.md
[value-tuple]: ../../../reference/types/tuple.md
[tuple]: ../../../reference/types/tuple.md
[range]: ../../../reference/types/range.md
[nullable]: ../../../reference/types/nullable.md
