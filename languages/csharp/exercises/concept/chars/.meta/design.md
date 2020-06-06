## Goal

The goal of this exercise is to teach the student the basics of the Concept of Chars in [C#][chars-docs].

## Learning objectives

- Know of the existence of the `char` type.
- Know what a char represents (a Unicode character).
- Know how to define a `char`.
- Know that a `char` is not the same as a single character string
- Know how to access a `char` in a string by index.
- Know of some basic `char` methods (like converting to uppercase).
- Know that `char`s are immutable.
- Know how to compare characters
- Know how to use a `StringBuilder`

## Out of scope

- Converting an integer to a character and vice versa.
- `System.Char` as a struct - alias for the simple `char` type
- Advanced unicode issues such as surrogates, text normalization, combining characters
- cultural considerations and invariants

## Concepts

This Concepts Exercise's Concepts are:

- `chars`: know of the existence of the `char` type; know that a `char` represents; know how to define a `char`; know how to access a `char` in a string by index; know of some basic `char` methods (like converting to uppercase).
- `StringBuilder`: know how to use this.

## Prequisites

This Exercise's prerequisites Concepts are:

- `strings`: know of the `string` type that will be iterated over and accessed by index.
- `for-loop` for loops (rather than foreach) are the best means of highlighting the relationship between strings and `char`s

## Representer

This exercise does not require any specific representation logic to be added to the [representer][representer].

## Analyzer

This exercise does not require any specific analyzer logic to be added to the [analyzer][analyzer].

[how-to-implement-a-concept-exercise]: https://github.com/exercism/v3/blob/master/docs/maintainers/generic-how-to-implement-a-concept-exercise.md
[implemented-exercises]: https://github.com/exercism/v3/tree/master/languages/csharp/exercises/concept/README.md#implemented-exercises
[reference]: https://github.com/exercism/v3/blob/master/languages/csharp/reference/README.md#reference-docs
[reference-char]: https://github.com/exercism/v3/blob/master/reference/types/char.md
[reference-example]: https://github.com/exercism/v3/blob/master/reference/types/string.md#implementations
[analyzer]: https://github.com/exercism/csharp-analyzer
[representer]: https://github.com/exercism/csharp-representer
[exercise-example]: https://github.com/exercism/v3/tree/master/languages/csharp/exercises/concept/numbers-floating-point
[design-example]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/concept/numbers/.meta/design.md
[config.json-example]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/concept/numbers/.meta/config.json
[concept-exercises]: https://github.com/exercism/v3/blob/master/docs/concept-exercises.md
