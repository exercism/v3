# C&#35; concept exercises

For C#'s v3 track, we'll be implementing [concept exercises][general-docs-concept-exercises].

## Already implemented exercises

These are the concept exercises that have currently been implemented, as well as the concepts they teach and their prerequisite concepts:

| exercise                                                            | concepts                                                       | prerequisites                                                  |
| ------------------------------------------------------------------- | -------------------------------------------------------------- | -------------------------------------------------------------- |
| [`numbers`][concept-exercise-numbers]                               | `basic-numbers`<br/>`basic-type-conversion`<br/>`conditionals` | -                                                              |
| [`numbers-floating-point`][concept-exercise-numbers-floating-point] | `floating-point-numbers`<br/>`loops`                           | `basic-numbers`<br/>`basic-type-conversion`<br/>`conditionals` |
| [`strings`][concept-exercise-strings]                               | `basic-strings`                                                | -                                                              |
| [`enums`][concept-exercise-enums]                                   | `basic-enums`                                                  | `basic-strings`                                                |
| [`dates`][concept-exercise-dates]                                   | `basic-dates`<br/>`basic-time`<br/>`string-formatting`         | `basic-numbers`<br/>`basic-strings`                            |
| [`bitwise-operations`][concept-exercise-bitwise-operations]         | `bitwise-operations`<br/>`advanced-enums`                      | `basic-enums`                                                  |

**⚠ Note ⚠**: The idea here is to use a `concept` name for the exercise/folder, but perhaps use some sort of "progression", so they will naturally become a sort of path to traverse. In this example, the `numbers` exercise only teaches basic number usage, and the `numbers-floating-point` exercise builds on that and digs deeper into floating-point numbers.

It's only important that it's reasonably easy to _find_ the exercise. It's okay if the name isn't perfect. We **will** iterate on this.

## Concept interpretation

Here is how the C# track has interpreted the following concept keywords. This should be synced across tracks.

| concept                  | interpretation                                                                                                                                                                                                                                                                                                     |
| ------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `basic-numbers`          | Know of the existence of the two most commonly used number types, `int` and `double`, and understand that the former represents whole numbers, and the latter floating-point numbers. Know of basic operators such as multiplication and comparison. Know where it's documented, or at least how to search for it. |
| `basic-strings`          | Know of the existence of the `string` type. Know of some basic functions (like looking up a character at a position, or slicing the string). Know how to do basic string formatting. Know where it's documented, or at least how to search for it.                                                                 |
| `basic-dates`            | Know of the existence of the `DateTime` type. Know of the individual, date-related properties. Know how to access the current date. Know how to compare dates. Know how to convert a `string` to a `DateTime` and vice versa. Know where it's documented, or at least how to search for it                         |
| `basic-enums`            | Know of the existence of the `enum` keyword. Know how to define enum members. Know how to convert a `string` to an `enum` and vice versa. Know where it's documented, or at least how to search for it                                                                                                             |
| `advanced-enums`         | Know how to define a "flags" enum. Know how to add, remove or check for flags.                                                                                                                                                                                                                                     |
| `basic-time`             | Know of the existence of the `DateTime` type. Know of the individual, time-related properties. Know where it's documented, or at least how to search for it.                                                                                                                                                       |
| `basic-type-conversion`  | Know that it is sometimes possible to convert from one type to another type.                                                                                                                                                                                                                                       |
| `conditionals`           | Know of the existence of conditional execution statements (such as the `if` statement).                                                                                                                                                                                                                            |
| `floating-point-numbers` | Know of the existing of the three floating point types: `double`, `float` and `decimal`. Know when to use which type.                                                                                                                                                                                              |
| `string-formatting`      | Know how to format a string. Know where it's documented, or at least how to search for it.                                                                                                                                                                                                                         |
|                          |
| `bitwise-operations`     | Know how to apply bitwise operations to numbers. Know where it's documented, or at least how to search for it.                                                                                                                                                                                                     |

This also indicates that for example `basic-strings` does **not** teach using custom formatting strings and that `basic-numbers` does **not** teach about checked/unchecked arithmetic.

## Contributing

Thanks for wanting to contribute to the C# track's concept exercises! Contributions are very welcome!

To contribute, please find and work on one of the [new exercise issues][issues-new-exercise] or [improve exercise issues][issues-improve-exercise].

[general-docs-concept-exercises]: ../../../../docs/concept-exercises.md
[reference]: ./reference.md
[concept-exercises]: ./concept
[concept-exercise-bitwise-operations]: ./concept/bitwise-operations
[concept-exercise-dates]: ./concept/dates
[concept-exercise-enums]: ./concept/enums
[concept-exercise-numbers-floating-point]: ./concept/numbers-floating-point
[concept-exercise-numbers]: ./concept/numbers
[concept-exercise-strings]: ./concept/strings
[issues-new-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3Atrack%2Fcsharp+label%3Atype%2Fnew-exercise+label%3Astatus%2Fhelp-wanted
[issues-improve-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3Atrack%2Fcsharp+label%3Atype%2Fimprove-exercise+label%3Astatus%2Fhelp-wanted
