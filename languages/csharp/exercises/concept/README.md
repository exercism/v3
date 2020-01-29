# C&#35; concept exercises

The concept exercises are based on this [list of concepts][reference-shared].

## Implemented exercises

These are the concept exercises that have currently been implemented, as well as the concepts they teach and their prerequisite concepts:

| exercise                                                            | concepts                                                       | prerequisites                                                  |
| ------------------------------------------------------------------- | -------------------------------------------------------------- | -------------------------------------------------------------- |
| [`numbers`][concept-exercise-numbers]                               | `numbers-basic`<br/>`type-conversion-basic`<br/>`conditionals` | -                                                              |
| [`numbers-floating-point`][concept-exercise-numbers-floating-point] | `numbers-floating-point`<br/>`loops`                           | `numbers-basic`<br/>`type-conversion-basic`<br/>`conditionals` |
| [`strings`][concept-exercise-strings]                               | `strings-basic`                                                | -                                                              |
| [`enums`][concept-exercise-enums]                                   | `enums-basic`                                                  | `strings-basic`                                                |
| [`dates`][concept-exercise-dates]                                   | `dates-basic`<br/>`time-basic`<br/>`strings-formatting`        | `numbers-basic`<br/>`strings-basic`                            |
| [`bitwise-operations`][concept-exercise-bitwise-operations]         | `bitwise-operations`<br/>`enums-advanced`                      | `enums-basic`                                                  |

**⚠ Note ⚠**: The idea here is to use a `concept` name for the exercise/folder, but perhaps use some sort of "progression", so they will naturally become a sort of path to traverse. In this example, the `numbers` exercise only teaches basic number usage, and the `numbers-floating-point` exercise builds on that and digs deeper into floating-point numbers.

It's only important that it's reasonably easy to _find_ the exercise. It's okay if the name isn't perfect. We **will** iterate on this.

## TODO

Thanks for wanting to contribute to the C# track's concept exercises! Contributions are very welcome!

To contribute, please find and work on one of the [new exercise issues][issues-new-exercise] or [improve exercise issues][issues-improve-exercise].

[reference-shared]: ../../reference/README.md
[reference]: ./reference.md
[concept-exercises]: ./concept
[concept-exercise-bitwise-operations]: ./bitwise-operations
[concept-exercise-dates]: ./dates
[concept-exercise-enums]: ./enums
[concept-exercise-numbers-floating-point]: ./numbers-floating-point
[concept-exercise-numbers]: ./numbers
[concept-exercise-strings]: ./strings
[issues-new-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3Atrack%2Fcsharp+label%3Atype%2Fnew-exercise+label%3Astatus%2Fhelp-wanted
[issues-improve-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3Atrack%2Fcsharp+label%3Atype%2Fimprove-exercise+label%3Astatus%2Fhelp-wanted
